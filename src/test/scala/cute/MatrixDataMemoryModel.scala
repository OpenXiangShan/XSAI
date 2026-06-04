package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import scala.collection.mutable.ArrayBuffer

/**
 * Matrix Data Memory Model for CUTE testing
 *
 * This model simulates matrix data memory with configurable matrix dimensions,
 * strides, and base addresses. It handles read/write requests and provides
 * reference computation for matrix multiplication (A*B+C).
 *
 * @param M Number of rows in matrices A and C, and result D
 * @param N Number of columns in matrix B and result D
 * @param K Number of columns in matrix A and rows in matrix B
 * @param StrideA Row stride in bytes for matrix A
 * @param StrideB Row stride in bytes for matrix B
 * @param StrideC Row stride in bytes for matrix C
 * @param StrideD Row stride in bytes for matrix D
 * @param BaseA Base address for matrix A
 * @param BaseB Base address for matrix B
 * @param BaseC Base address for matrix C
 * @param BaseD Base address for matrix D
 * @param EA Element size in bytes for matrix A
 * @param EB Element size in bytes for matrix B
 * @param EC Element size in bytes for matrix C
 * @param ED Element size in bytes for matrix D
 */
class MatrixDataMemoryModel(
  val M: Int,
  val N: Int,
  val K: Int,
  val StrideA: Long,
  val StrideB: Long,
  val StrideC: Long,
  val StrideD: Long,
  val BaseA: Long,
  val BaseB: Long,
  val BaseC: Long,
  val BaseD: Long,
  val EA: Int,
  val EB: Int,
  val EC: Int,
  val ED: Int,
  val beatBytes: Int = 64
) extends CuteConsts {

  // Matrix storage as 1D arrays
  val A = new Array[Long](M * K)
  val B = new Array[Long](N * K)
  val C = new Array[Long](M * N)
  val D = new Array[Long](M * N)

  // Reference result storage for A*B+C
  val ReferenceResult = new Array[Long](M * N)

  // Track whether D has been written
  var DInitialized = false

  /**
   * Initialize matrices A, B, C with random data
   * A and B: values 0-11
   * C: values 0-1024
   * D: initialized to 0 (empty)
   */
  def initialize(random: scala.util.Random = new scala.util.Random(42)): Unit = {
    // Initialize matrix A (M x K) with values 0-11
    for (i <- 0 until M * K) {
      A(i) = 0x10 + random.nextInt(0xf)
    }

    // Initialize matrix B (N x K, transposed layout) with values 0-11
    for (i <- 0 until N * K) {
      B(i) = 0x20 + random.nextInt(0xf)
    }

    // Initialize matrix C (M x N) with values 0-1024
    for (i <- 0 until M * N) {
      C(i) = 0x30000000 + random.nextInt(0xfff)
    }

    // Initialize matrix D to 0 (empty array)
    for (i <- 0 until M * N) {
      D(i) = 0
    }

    DInitialized = false

    // Compute reference result A*B+C
    computeReferenceResult()

    println(s"[MatrixMemoryModel] Initialized matrices:")
    println(s"  A: ${M}x${K}, stride=$StrideA bytes, base=0x${BaseA.toHexString}, elem_size=$EA bytes")
    println(s"  B: ${N}x${K}, stride=$StrideB bytes, base=0x${BaseB.toHexString}, elem_size=$EB bytes")
    println(s"  C: ${M}x${N}, stride=$StrideC bytes, base=0x${BaseC.toHexString}, elem_size=$EC bytes")
    println(s"  D: ${M}x${N}, stride=$StrideD bytes, base=0x${BaseD.toHexString}, elem_size=$ED bytes")
  }

  /**
   * Compute reference result A*B+C using standard matrix multiplication
   */
  def computeReferenceResult(): Unit = {
    for (m <- 0 until M) {
      for (n <- 0 until N) {
        var sum = 0L
        for (k <- 0 until K) {
          val a_val = A(m * K + k)
          val b_val = B(n * K + k) // B is stored in transposed layout
          sum += a_val * b_val
        }
        val c_val = C(m * N + n)
        ReferenceResult(m * N + n) = sum + c_val
      }
    }
    println("[MatrixMemoryModel] Reference result A*B+C computed")
  }

  /**
   * Route address to corresponding matrix
   * @return Tuple(matrix_name, array, stride, elem_size, base, rows, cols)
   */
  private def routeAddress(addr: Long): Option[(String, Array[Long], Long, Int, Long, Int, Int)] = {
    val addrAligned = addr & 0xffffffffffffffffL

    if (addrAligned >= BaseA && addrAligned < BaseA + M * StrideA) {
      Some(("A", A, StrideA, EA, BaseA, M, K))
    } else if (addrAligned >= BaseB && addrAligned < BaseB + N * StrideB) {
      Some(("B", B, StrideB, EB, BaseB, N, K))
    } else if (addrAligned >= BaseC && addrAligned < BaseC + M * StrideC) {
      Some(("C", C, StrideC, EC, BaseC, M, N))
    } else if (addrAligned >= BaseD && addrAligned < BaseD + M * StrideD) {
      Some(("D", D, StrideD, ED, BaseD, M, N))
    } else {
      None
    }
  }

  /**
   * Calculate 2D indices from 1D address
   * @return Tuple(row, col, element_index)
   */
  private def calculateIndices(addr: Long, base: Long, stride: Long, elemSize: Int, rows: Int, cols: Int): (Int, Int, Int) = {
    val offset = addr - base
    val row = (offset / stride).toInt
    val colOffset = offset % stride
    val col = (colOffset / elemSize).toInt
    val elemIndex = row * cols + col
    (row, col, elemIndex)
  }

  /**
   * Check if access crosses row boundary
   * @return true if crossing row boundary
   */
  private def checkRowCrossing(addr: Long, base: Long, stride: Long, bytes: Int): Boolean = {
    val offset = addr - base
    val startRow = offset / stride
    val endOffset = offset + bytes - 1
    val endRow = endOffset / stride
    startRow != endRow
  }

  /**
   * Handle read request
   * @param addr Physical address to read from
   * @param bytes Number of bytes to read
   * @return Tuple(data, valid, warning_message)
   */
  def handleRead(addr: Long, bytes: Int): (Long, Boolean, Option[String]) = {
    routeAddress(addr) match {
      case Some((name, array, stride, elemSize, base, rows, cols)) =>
        // Check for row crossing
        val crossing = checkRowCrossing(addr, base, stride, bytes)
        val warning = if (crossing) {
          Some(s"WARNING: Read crosses row boundary in matrix $name at addr 0x${addr.toHexString}")
        } else None

        // Calculate how many elements to read
        val numElements = bytes / elemSize
        val (row, col, startIndex) = calculateIndices(addr, base, stride, elemSize, rows, cols)

        // Pack data in little-endian order
        var resultData: Long = 0L
        var valid = true

        for (i <- 0 until numElements) {
          val elemIndex = startIndex + i
          if (elemIndex < array.length) {
            val elemValue = array(elemIndex)
            // Pack element in little-endian byte order
            resultData |= (elemValue & ((1L << (elemSize * 8)) - 1)) << (i * elemSize * 8)
          } else {
            valid = false
          }
        }

        // Log read request with detailed information
        println(s"[ScalaMemoryModel] READ: addr=0x${addr.toHexString}, bytes=$bytes, matrix=$name, row=$row, col=$col, startIndex=$startIndex, numElements=$numElements")
        println(s"[ScalaMemoryModel] READ: data=0x${resultData.toHexString} (little-endian packed)")

        (resultData, valid, warning)

      case None =>
        println(s"[ScalaMemoryModel] READ ERROR: addr=0x${addr.toHexString}, bytes=$bytes - Address not mapped to any matrix")
        (0L, false, Some(s"ERROR: Read address 0x${addr.toHexString} not mapped to any matrix"))
    }
  }

  /**
   * Handle read request with BigInt support for large data widths (up to 512 bits)
   * @param addr Physical address to read from
   * @param bytes Number of bytes to read
   * @return Tuple(data as BigInt, valid, warning_message)
   */
  def handleReadBigInt(addr: Long, bytes: Int): (scala.math.BigInt, Boolean, Option[String]) = {
    routeAddress(addr) match {
      case Some((name, array, stride, elemSize, base, rows, cols)) =>
        // Check for row crossing
        val crossing = checkRowCrossing(addr, base, stride, bytes)
        val warning = if (crossing) {
          Some(s"WARNING: Read crosses row boundary in matrix $name at addr 0x${addr.toHexString}")
        } else None

        // Calculate how many elements to read
        val numElements = bytes / elemSize
        val (row, col, startIndex) = calculateIndices(addr, base, stride, elemSize, rows, cols)

        // Pack data in little-endian order using BigInt
        var resultData: scala.math.BigInt = scala.math.BigInt(0)
        var valid = true

        for (i <- 0 until numElements) {
          val elemIndex = startIndex + i
          if (elemIndex < array.length) {
            val elemValue = array(elemIndex)
            // Pack element in little-endian byte order
            val elemBigInt = scala.math.BigInt(elemValue)
            resultData = resultData | (elemBigInt << (i * elemSize * 8))
          } else {
            valid = false
          }
        }

        // Log read request with detailed information
        println(s"[ScalaMemoryModel] READ: addr=0x${addr.toHexString}, bytes=$bytes, matrix=$name, row=$row, col=$col, startIndex=$startIndex, numElements=$numElements")
        println(s"[ScalaMemoryModel] READ: data=0x${resultData.toString(16)} (little-endian packed, BigInt)")

        (resultData, valid, warning)

      case None =>
        println(s"[ScalaMemoryModel] READ ERROR: addr=0x${addr.toHexString}, bytes=$bytes - Address not mapped to any matrix")
        (scala.math.BigInt(0), false, Some(s"ERROR: Read address 0x${addr.toHexString} not mapped to any matrix"))
    }
  }

  /**
   * Handle write request
   * @param addr Physical address to write to
   * @param data Data to write
   * @param bytes Number of bytes to write
   * @return Tuple(success, warning_message)
   */
  def handleWrite(addr: Long, data: Long, bytes: Int): (Boolean, Option[String]) = {
    routeAddress(addr) match {
      case Some((name, array, stride, elemSize, base, rows, cols)) =>
        // Check for row crossing
        val crossing = checkRowCrossing(addr, base, stride, bytes)
        val warning = if (crossing) {
          Some(s"WARNING: Write crosses row boundary in matrix $name at addr 0x${addr.toHexString}")
        } else None

        // Calculate how many elements to write
        val numElements = bytes / elemSize
        val (row, col, startIndex) = calculateIndices(addr, base, stride, elemSize, rows, cols)

        // Collect element values for logging
        val elemValues = new Array[Long](numElements)

        // Unpack data in little-endian order and write to array
        for (i <- 0 until numElements) {
          val elemIndex = startIndex + i
          if (elemIndex < array.length) {
            // Extract element in little-endian byte order
            val elemValue = (data >> (i * elemSize * 8)) & ((1L << (elemSize * 8)) - 1)
            array(elemIndex) = elemValue
            elemValues(i) = elemValue
          }
        }

        // Log write request with detailed information
        println(s"[ScalaMemoryModel] WRITE: addr=0x${addr.toHexString}, bytes=$bytes, matrix=$name, row=$row, col=$col, startIndex=$startIndex, numElements=$numElements")
        println(s"[ScalaMemoryModel] WRITE: data=0x${data.toHexString} (little-endian packed)")
        println(s"[ScalaMemoryModel] WRITE: elements=[${elemValues.mkString(", ")}]")

        // Mark D as initialized if writing to D
        if (name == "D") {
          DInitialized = true
        }

        (true, warning)

      case None =>
        println(s"[ScalaMemoryModel] WRITE ERROR: addr=0x${addr.toHexString}, bytes=$bytes, data=0x${data.toHexString} - Address not mapped to any matrix")
        (false, Some(s"ERROR: Write address 0x${addr.toHexString} not mapped to any matrix"))
    }
  }

  /**
   * Handle write request with BigInt support for large data widths (up to 256 bits)
   * @param addr Physical address to write to
   * @param data Data to write (as BigInt)
   * @param bytes Number of bytes to write
   * @return Tuple(success, warning_message)
   */
  def handleWriteBigInt(addr: Long, data: scala.math.BigInt, bytes: Int): (Boolean, Option[String]) = {
    routeAddress(addr) match {
      case Some((name, array, stride, elemSize, base, rows, cols)) =>
        // Check for row crossing
        val crossing = checkRowCrossing(addr, base, stride, bytes)
        val warning = if (crossing) {
          Some(s"WARNING: Write crosses row boundary in matrix $name at addr 0x${addr.toHexString}")
        } else None

        // Calculate how many elements to write
        val numElements = bytes / elemSize
        val (row, col, startIndex) = calculateIndices(addr, base, stride, elemSize, rows, cols)

        // Collect element values for logging
        val elemValues = new Array[Long](numElements)
        val elemMask = (BigInt(1) << (elemSize * 8)) - 1  // Mask for element size

        // Unpack data in little-endian order and write to array
        for (i <- 0 until numElements) {
          val elemIndex = startIndex + i
          if (elemIndex < array.length) {
            // Extract element in little-endian byte order
            val elemValue = ((data >> (i * elemSize * 8)) & elemMask).toLong
            array(elemIndex) = elemValue
            elemValues(i) = elemValue
          }
        }

        // Log write request with detailed information
        println(s"[ScalaMemoryModel] WRITE: addr=0x${addr.toHexString}, bytes=$bytes, matrix=$name, row=$row, col=$col, startIndex=$startIndex, numElements=$numElements")
        println(s"[ScalaMemoryModel] WRITE: data=0x${data.toString(16)} (little-endian packed, BigInt)")
        println(s"[ScalaMemoryModel] WRITE: elements=[${elemValues.mkString(", ")}]")

        // Mark D as initialized if writing to D
        if (name == "D") {
          DInitialized = true
        }

        (true, warning)

      case None =>
        println(s"[ScalaMemoryModel] WRITE ERROR: addr=0x${addr.toHexString}, bytes=$bytes, data=0x${data.toString(16)} - Address not mapped to any matrix")
        (false, Some(s"ERROR: Write address 0x${addr.toHexString} not mapped to any matrix"))
    }
  }

  /**
   * Compare D array with reference result
   * @return Tuple(all_match, mismatch_count, first_mismatch_index)
   */
  def compareWithReference(): (Boolean, Int, Option[Int]) = {
    if (!DInitialized) {
      println("[MatrixMemoryModel] WARNING: D matrix not written, cannot compare")
      return (false, -1, None)
    }

    var mismatchCount = 0
    var firstMismatch: Option[Int] = None

    for (i <- 0 until M * N) {
      val dVal = D(i)
      val refVal = ReferenceResult(i)
      if (dVal != refVal) {
        if (firstMismatch.isEmpty) {
          firstMismatch = Some(i)
        }
        mismatchCount += 1
      }
    }

    val allMatch = mismatchCount == 0

    if (allMatch) {
      println(s"[MatrixMemoryModel] SUCCESS: D matrix matches reference result A*B+C")
    } else {
      println(s"[MatrixMemoryModel] FAILURE: D matrix has $mismatchCount mismatches with reference result")
      firstMismatch.foreach { idx =>
        val m = idx / N
        val n = idx % N
        println(s"  First mismatch at [$m][$n]: D=${D(idx)}, Reference=${ReferenceResult(idx)}")
      }
    }

    (allMatch, mismatchCount, firstMismatch)
  }

  /**
   * Print matrix contents for debugging
   */
  def printMatrix(name: String): Unit = {
    val (array, rows, cols) = name match {
      case "A" => (A, M, K)
      case "B" => (B, N, K)
      case "C" => (C, M, N)
      case "D" => (D, M, N)
      case "Reference" => (ReferenceResult, M, N)
      case _ => throw new IllegalArgumentException(s"Unknown matrix: $name")
    }

    println(s"\nMatrix $name ($rows x $cols):")
    for (i <- 0 until rows) {
      for (j <- 0 until cols) {
        print(s"${array(i * cols + j)}%4d ")
      }
      println()
    }
    println()
  }

  /**
   * Print a summary of all matrices
   */
  def printSummary(): Unit = {
    println(s"\n[MatrixMemoryModel] Summary:")
    println(s"  Dimensions: M=$M, N=$N, K=$K")
    println(s"  Base addresses: A=0x${BaseA.toHexString}, B=0x${BaseB.toHexString}, C=0x${BaseC.toHexString}, D=0x${BaseD.toHexString}")
    println(s"  Strides: A=$StrideA, B=$StrideB, C=$StrideC, D=$StrideD bytes")
    println(s"  Element sizes: A=$EA, B=$EB, C=$EC, D=$ED bytes")
    println(s"  D initialized: $DInitialized")
  }
}

/**
 * Companion object with default configurations
 */
object MatrixDataMemoryModel {
  /**
   * Create a default memory model matching the CuteTestHarness configuration
   */
  def apply(
    M: Int = 128,
    N: Int = 128,
    K: Int = 64,
    beatBytes: Int = 64
  ): MatrixDataMemoryModel = {
    new MatrixDataMemoryModel(
      M = M,
      N = N,
      K = K,
      StrideA = 64L,
      StrideB = 64L,
      StrideC = 128L * 4,
      StrideD = 128L * 4,
      BaseA = 0x10000000L,
      BaseB = 0x20000000L,
      BaseC = 0x30000000L,
      BaseD = 0x40000000L,
      EA = 1,  // 1 byte for int8
      EB = 1,  // 1 byte for int8
      EC = 4,  // 4 bytes for int32
      ED = 4,  // 4 bytes for int32
      beatBytes = beatBytes
    )
  }

  /**
   * Create a custom memory model with specific parameters
   */
  def custom(
    M: Int,
    N: Int,
    K: Int,
    StrideA: Long,
    StrideB: Long,
    StrideC: Long,
    StrideD: Long,
    BaseA: Long,
    BaseB: Long,
    BaseC: Long,
    BaseD: Long,
    EA: Int,
    EB: Int,
    EC: Int,
    ED: Int,
    beatBytes: Int = 64
  ): MatrixDataMemoryModel = {
    new MatrixDataMemoryModel(
      M = M,
      N = N,
      K = K,
      StrideA = StrideA,
      StrideB = StrideB,
      StrideC = StrideC,
      StrideD = StrideD,
      BaseA = BaseA,
      BaseB = BaseB,
      BaseC = BaseC,
      BaseD = BaseD,
      EA = EA,
      EB = EB,
      EC = EC,
      ED = ED,
      beatBytes = beatBytes
    )
  }
}

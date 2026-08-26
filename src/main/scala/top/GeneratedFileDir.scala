package top

object GeneratedFileDir {
  def value: String = sys.env.getOrElse("GENERATED_DIR", "./build")
}

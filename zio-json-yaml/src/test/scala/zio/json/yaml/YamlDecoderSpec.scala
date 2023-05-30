package zio.json.yaml

import zio.json.ValidationAssertions.isSucceeded
import zio.json.yaml.YamlEncoderSpec.{ Example, ex1, ex1Yaml, ex1Yaml2 }
import zio.test.Assertion.equalTo
import zio.test._

object YamlDecoderSpec extends ZIOSpecDefault {

  val spec: Spec[Environment, Any] =
    suite("YamlDecoderSpec")(
      test("object root") {
        assert(ex1Yaml.fromYaml[Example])(
          isSucceeded(equalTo(ex1))
        )
      },
      test("object root, different indentation") {
        assert(ex1Yaml2.fromYaml[Example])(
          isSucceeded(equalTo(ex1))
        )
      },
      test("scalar root") {
        assert("hello".fromYaml[String])(isSucceeded(equalTo("hello")))
      },
      test("bool root") {
        assert("yes".fromYaml[Boolean])(isSucceeded(equalTo(true)))
      },
      test("float root") {
        assert("3.14".fromYaml[Double])(isSucceeded(equalTo(3.14)))
      },
      test("sequence root") {
        assert("- a\n- b\n- c".fromYaml[Vector[String]])(isSucceeded(equalTo(Vector("a", "b", "c"))))
      }
    )
}

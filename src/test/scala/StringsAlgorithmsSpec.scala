import org.scalatest._
import flatspec._
import matchers._
import strings.StringsAlgorithms

class StringsAlgorithmsSpec extends AnyFlatSpec with should.Matchers {

  "isUnique" should "return true when abcd is passed in parameter" in {
    StringsAlgorithms.isUnique("abcd") should be (true)
  }

  "isUnique" should "return true when unique is passed in parameter" in {
    StringsAlgorithms.isUnique("unique") should be (false)
  }

  "checkPermutation" should "return true when god and dog are passed in parameter" in {
    StringsAlgorithms.checkPermutation("god", "dog") should be (true)
  }

  "checkPermutation" should "return false when god and odg are passed in parameter" in {
    StringsAlgorithms.checkPermutation("god", "odg") should be (false)
  }

  "isPalindromePermutation" should "return true when TactCoa is passed in parameter" in {
    StringsAlgorithms.isPalindromePermutation("TactCoa") should be (true)
  }

  "isPalindromePermutation" should "return false when palindrome is passed in parameter" in {
    StringsAlgorithms.isPalindromePermutation("palindrome") should be (false)
  }

  "isPalindromePermutationOptimized" should "return true when TactCoa is passed in parameter" in {
    StringsAlgorithms.isPalindromePermutationOptimized("TactCoa") should be (true)
  }

  "isPalindromePermutationOptimized" should "return false when palindrome is passed in parameter" in {
    StringsAlgorithms.isPalindromePermutationOptimized("palindrome") should be (false)
  }
}

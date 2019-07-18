package tutorial.webapp

import utest._

import org.querki.jquery._

object TutorialTest extends TestSuite {
  // InitializeApp
  TutorialApp.setupUI()

  def tests = Tests {
    'HelloWorld - {
      assert($("p:contains('Hello World')").length == 1)
    }

    'ClickedButton - {
      def messageCount =
        $("p:contains('Snakes!')").length

      val button = $("button:contains('Click me!')")
      assert(button.length == 1)
      assert(messageCount == 0)

      for(c <- 1 to 5) {
        button.click()
        assert(messageCount == c)
      }
    }
  }
}

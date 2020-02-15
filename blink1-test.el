(require 'buttercup)
(require 'blink1)

(describe "blink1--rgb-color"
  (it "coerces a non-RGB value"
    (expect (blink1--rgb-color "red") :to-equal '(255 0 0)))
  (it "preserves an existing RGB value"
    (expect (blink1--rgb-color '(255 0 0)) :to-equal '(255 0 0))))

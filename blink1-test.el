(require 'buttercup)
(require 'blink1)

(describe "blink1--rbg-string-p"
  (it "identifies six-digit strings with a hash"
    (expect (blink1--rbg-string-p "#ff0000") :to-be t)))

(describe "blink1--rgb-color"
  (it "coerces a non-RGB value"
    (expect (blink1--rgb-color "red") :to-equal '(255 0 0)))
  (it "preserves an existing RGB value"
    (expect (blink1--rgb-color '(255 0 0)) :to-equal '(255 0 0))))

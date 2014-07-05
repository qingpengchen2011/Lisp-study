(load "huffman.scm")
(load "114-exercise-2.69.scm")

(define song-tree 
	(generate-huffman-tree '((A 1) (NA 16) (BOOM 1) (SHA 3)
				 (GET 2) (YIP 9) (JOB 2) (WAH 1))))

(define (encode-song song-message)
    	(encode song-message song-tree))
(define sample-song 
	'(get a job
	  sha na na na na na na na na 
	  get a job
	  sha na na na na na na na na
          wah yip yip yip yip yip yip yip yip yip
          sha boom))



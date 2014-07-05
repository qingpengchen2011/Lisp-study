(load "huffman.scm")

(define (generate-huffman-tree pairs)
    (define (successive-merge tree-mode-set)
        (cond ((= (length tree-mode-set) 0) 
		'())
              ((= 1 (length tree-mode-set))
		(car tree-mode-set))
              (else
            	(let ((new-node (make-code-tree (car tree-mode-set)
					    (cadr tree-mode-set))))
	             (successive-merge (adjoin-set new-node (cddr tree-mode-set)))))))
    (successive-merge (make-leaf-set pairs)))

(require-extension (srfi 1))
(require-extension (srfi 6))
(require-extension (srfi 9))
(require-extension (srfi 11))

(module iasylum/packrat
         (parse-result?
	   parse-result-successful?
	   parse-result-semantic-value
	   parse-result-next
	   parse-result-error

	   parse-results?
	   parse-results-position
	   parse-results-base
	   parse-results-next

	   parse-error?
	   parse-error-position
	   parse-error-expected
	   parse-error-messages

	   make-parse-position
	   parse-position?
	   parse-position-file
	   parse-position-line
	   parse-position-column

	   top-parse-position
	   update-parse-position
	   parse-position->string

	   ;;empty-results
	   ;;make-results

	   make-error-expected
	   make-error-message
	   make-result
	   make-expected-result
	   make-message-result

	   prepend-base
	   prepend-semantic-value

	   base-generator->results
	   results->result

	   parse-position>?
	   parse-error-empty?
	   merge-parse-errors
	   merge-result-errors

	   parse-results-token-kind
	   parse-results-token-value

	   packrat-check-base
	   packrat-check
	   packrat-or
	   packrat-unless

	   packrat-parser object->external-representation)

  (include "packrat/packrat-code.scm"))
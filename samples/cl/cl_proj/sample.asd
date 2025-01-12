(asdf:defsystem "sample"
		:version "0.1"
		:depends-on ()
		:components ((:module "src"
				      :components 
				      	(
					 (:file "main")
					)))
		:description "A sample")


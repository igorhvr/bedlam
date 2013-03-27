;;; Code by Igor Hjelmstrom Vinhas Ribeiro - this is licensed under GNU GPL v2.

(require-extension (lib iasylum/jcode))

(module iasylum/jmx
  (jmx-identifier get-standard-mbean-server
   build-jmx-thunk-bean add-thunk-to-jmx
   build-jmx-one-string-parameter-function-bean
   add-one-string-parameter-function-to-jmx
   )

  (define (jmx-identifier category name) (j "new javax.management.ObjectName(category, \"type\", name);" `((category ,(->jstring category)) (name ,(->jstring name)))))

  (define (get-standard-mbean-server) (j "java.lang.management.ManagementFactory.getPlatformMBeanServer();"))
  
  (define (build-jmx-thunk-bean thunk)
    (j "new iu.jmx.Thunk(thk);" `((thk ,(->jobject thunk)))))
  
  
  (define (add-thunk-to-jmx thunk category name)
    (let* ((server (get-standard-mbean-server))
           (identifier (jmx-identifier category name))
           (bean (build-jmx-thunk-bean thunk)))
      (j "server.registerMBean(bean, identifier);" `((server ,server) (identifier ,identifier) (bean ,bean)))))
  
  (define (build-jmx-one-string-parameter-function-bean function)
    (j "new iu.jmx.OneStringParameterFunction(func);" `((func ,(->jobject function)))))
  
  
  (define (add-one-string-parameter-function-to-jmx function category name)
    (let* ((server (get-standard-mbean-server))
           (identifier (jmx-identifier category name))
           (bean (build-jmx-one-string-parameter-function-bean function)))
      (j "server.registerMBean(bean, identifier);" `((server ,server) (identifier ,identifier) (bean ,bean)))))

  (define (self-test)
    (define (test-thunk) "123") 
    (define (test-function p) (string-append "bla" p "ble"))
    (add-thunk-to-jmx test-thunk "CategoryHere" "TestingNowThunk")
    (add-one-string-parameter-function-to-jmx test-function "CatBla" "TestingNowOneStringParameterFunction"))
  )

  
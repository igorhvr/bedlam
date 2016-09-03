;;
;; Use like:
;;
;; (schedule-job-easier (lambda () (d/n "Hello world!")) "0 * * * * ?")
;;
;; See also http://www.quartz-scheduler.org/api/previous_versions/1.8.5/org/quartz/CronExpression.html
;;
(define (schedule-job-easier thunk cron-expression)
  (let* ((job-group (uuid-string))
         (job-name (uuid-string))
         (trigger-group (uuid-string))
         (trigger-name (uuid-string))
         (scheduler (create-scheduler))
         (job (create-quartz-job-from-closure job-group job-name thunk))
         (trigger (create-quartz-cron-trigger trigger-group trigger-name cron-expression)))
    (schedule-job scheduler job trigger)))

(define (create-scheduler)
  (j
   (quote-convert
      (apply format
             (cons 
      "
       ~a = new Properties();
       ~a.setProperty('org.quartz.scheduler.instanceName', 'MyScheduler');
       ~a.setProperty('org.quartz.scheduler.instanceId', '1');
       ~a.setProperty('org.quartz.scheduler.rmi.export', 'false');
       ~a.setProperty('org.quartz.scheduler.rmi.proxy', 'false');       
       ~a.setProperty('org.quartz.threadPool.class', 'org.quartz.simpl.SimpleThreadPool');
       ~a.setProperty('org.quartz.threadPool.threadCount', '10');
       ~a.setProperty('org.quartz.jobStore.class', 'org.quartz.simpl.RAMJobStore');
       org.quartz.Scheduler scheduler = new org.quartz.impl.StdSchedulerFactory(~a).getDefaultScheduler();
       scheduler.start();
       ~a = null;
       scheduler;"
      (make-list 10 (random-var)))))))

(define (create-quartz-job-from-closure job-group job-name  cl)
  (j
    (quote-convert
       "import org.quartz.JobDetail;
        String closureKey='CLOSURE_KEY';
        import org.apache.log4j.Logger;
        import sisc.interpreter.Interpreter;
        import sisc.interpreter.Context;
        public class ClosureJob implements org.quartz.Job {
           static final Logger log = Logger.getLogger('closureJobLogger');
           public void execute(org.quartz.JobExecutionContext context) {
               log.debug('Starting scheme job.');
               Object closure=context.getJobDetail().getJobDataMap().get('CLOSURE_KEY');
               Interpreter current = Context.currentInterpreter();
               if(current==null) {
                   current =
                       sisc.interpreter.Context.enter(sisc.interpreter.Context.getDefaultAppContext());

               }
               current.eval(closure,new sisc.data.Value[0]);
               log.info('Finished scheme job.');
           }
        };
        JobDetail job = new JobDetail(jobname, jobgroup, ClosureJob.class);
        job.getJobDataMap().put(closureKey,cl);job;")
    `((jobname ,(->jstring job-name))
      (jobgroup ,(->jstring job-group))
      (cl ,(java-wrap cl)))))

(define (create-quartz-cron-trigger trigger-group trigger-name cron-expression)
  (j "org.quartz.Trigger trigger = new org.quartz.CronTrigger(triggername, triggergroup, cronexpression); trigger;"
     `((triggername ,(->jstring trigger-name)) (triggergroup ,(->jstring trigger-group))
       (cronexpression ,(->jstring cron-expression)))))

(define (schedule-job scheduler job trigger)
  (j "Object r = scheduler.scheduleJob(job, trigger); scheduler.start();r;"
     `((scheduler ,scheduler) (job ,job) (trigger ,trigger))))

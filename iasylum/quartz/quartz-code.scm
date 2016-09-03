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

(define current-scheduler-id (make-atomic-long 1))

(define create-scheduler
  (lambda* ((thread-count: thread-count 10))
  (j
   (quote-convert
      (let ((vname (random-var)))
        (string-append* 
      "
       " vname " = new Properties();
       " vname ".setProperty('org.quartz.scheduler.instanceName', '" vname "');
       " vname ".setProperty('org.quartz.scheduler.instanceId', '" (get-and-inc-atomic-long! current-scheduler-id) "');
       " vname ".setProperty('org.quartz.scheduler.rmi.export', 'false');
       " vname ".setProperty('org.quartz.scheduler.rmi.proxy', 'false');       
       " vname ".setProperty('org.quartz.threadPool.class', 'org.quartz.simpl.SimpleThreadPool');
       " vname ".setProperty('org.quartz.threadPool.threadCount', '" thread-count "');
       " vname ".setProperty('org.quartz.jobStore.class', 'org.quartz.simpl.RAMJobStore');
       org.quartz.Scheduler scheduler = new org.quartz.impl.StdSchedulerFactory(" vname ").getDefaultScheduler();
       scheduler.start();
       " vname " = null;
       scheduler;"))))))

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

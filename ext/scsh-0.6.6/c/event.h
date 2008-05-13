enum event_enum { KEYBOARD_INTERRUPT_EVENT,
		  IO_READ_COMPLETION_EVENT, IO_WRITE_COMPLETION_EVENT,
		  ALARM_EVENT,
		  OS_SIGNAL_EVENT, ERROR_EVENT, NO_EVENT };

extern bool s48_add_pending_fd(int fd, bool is_input);
extern bool s48_remove_fd(int fd);

extern long s48_schedule_alarm_interrupt(long delta);
extern void s48_start_alarm_interrupts(void);
extern void s48_stop_alarm_interrupts(void);

extern long s48_run_time(long *mseconds);
extern long s48_real_time(long *mseconds);
extern int  s48_wait_for_event(long max_wait, bool is_minutes);
extern int  s48_get_next_event(long *ready_fd, long *status);

/* these are here only for the CHEAP_TIME() macro */
#define TICKS_PER_SECOND 1000	/* clock resolution */
#define POLLS_PER_SECOND   20   /* how often we poll */
#define TICKS_PER_POLL  (TICKS_PER_SECOND / POLLS_PER_SECOND)

extern long s48_current_time;
#define CHEAP_TIME()  (s48_current_time * TICKS_PER_POLL)

/*
 * Fix (HCC) NOTE_EVENT so that it will act like a single
 * statement.
 */
#define	NOTE_EVENT					\
	do {						\
		s48_Spending_eventsPS = 1;		\
		s48_Spending_interruptPS = 1;		\
	} while (0)


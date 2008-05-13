#include <stdio.h>
#include "prescheme.h"
#include "scheme48vm-prelude.h"

static char add_more_channels(long);
static void enqueue_input_channelB(long, long);
static void enqueue_output_channelB(long, long);
static long close_channelB(long);
static void push_continuationB(char *, long);
static long enter_string(char*, long);
static char HcleanerB2200(long);
static void push_exception_continuationB(long, long);
static long make_registered_channel(long, long, long, long, long*);
static long channel_close_error(long, long, long);
static void HtopD9001(void);
static long Hlookup2142(long, long, long);
static long Hlookup2123(long, long, long);
static void HtopD8994(void);
static long collect_saving_temps(long, long, long*);
void s48_set_extension_valueB(long);
void s48_note_event(void);
long s48_exported_bindings(void);
char * s48_set_gc_roots_baseB(char **);
char s48_release_gc_roots_baseB(char *, char *);
void s48_disable_interruptsB(void);
void s48_push_gc_rootsB(char *, long);
void s48_register_gc_rootB(char *);
void s48_stack_setB(long, long);
long s48_stack_ref(long);
void s48_push(long);
long s48_resetup_external_exception(long, long);
char s48_pop_gc_rootsB(void);
void s48_enable_interruptsB(void);
void s48_mark_traced_channels_closedB(void);
void s48_set_os_signals(long);
long s48_set_channel_os_index(long, long);
long s48_cleaned_imported_bindings(void);
long s48_copy_symbol_table(void);
void s48_setup_external_exception(long, long);
void s48_close_channel(long);
char s48_warn_about_undefined_imported_bindings(void);
void s48_define_exported_binding(char*, long);
long s48_add_channel(long, long, long);
long s48_get_imported_binding(char*);
long s48_allocate_stob(long, long);
void s48_initialize_vm(char *, long);
long s48_restart(long, long);
long s48_call_startup_procedure(char**, long);
static long Spending_interruptsS;
static long Snumber_of_channelsS;
static long *Svm_channelsS;
static long Spending_input_channels_headS;
static long Spending_input_channels_tailS;
static long Spending_output_channels_headS;
static long Spending_output_channels_tailS;
static char * Sstack_beginS;
static char * Sstack_endS;
static char * SstackS;
static char * Sstack_limitS;
static long ScontS;
static long Sbottom_of_stackS;
static char Sstack_warningPS;
static long SenvS;
static long StemplateS;
static char * Scode_pointerS;
static long SvalS;
static long Sexception_handlersS;
static long Sinterrupt_handlersS;
static long Scurrent_threadS;
static long Ssession_dataS;
static long Sfinalizer_alistS;
static long Sfinalize_theseS;
static long Senabled_interruptsS;
static long Sinterrupted_templateS;
static long Sinterrupt_templateS;
static long Sexception_templateS;
static long Ssaved_pcS;
static long Slosing_opcodeS;
static long Sos_signal_listS;
static char Sexternal_exceptionPS;
static long Sexternal_exception_nargsS;
static long Simported_bindingsS;
static long Sexported_bindingsS;
static long Sthe_symbol_tableS;
static char * Sexternal_root_stackS;
static char * Sexternal_root_stack_baseS;
static char * Spermanent_external_rootsS;
static void (*Sgc_root_procS)(void);
static void (*Spost_gc_cleanupS)(void);
char s48_Spending_interruptPS;
long s48_Sextension_valueS;
long s48_Scallback_return_stack_blockS;
char s48_Spending_eventsPS;

static char add_more_channels(long index_5X)
{
  long arg0K0;
  long i_11X;
  long i_10X;
  long *new_vm_channels_9X;
  long new_count_8X;
  long y_7X;
  long x_6X;
 {  x_6X = 1 + index_5X;
  y_7X = 8 + (Snumber_of_channelsS);
  if ((x_6X < y_7X)) {
    arg0K0 = y_7X;
    goto L2398;}
  else {
    arg0K0 = x_6X;
    goto L2398;}}
 L2398: {
  new_count_8X = arg0K0;
  new_vm_channels_9X = (long*)malloc(sizeof(long) * new_count_8X);
  if ((NULL == new_vm_channels_9X)) {
    return 0;}
  else {
    arg0K0 = 0;
    goto L2410;}}
 L2410: {
  i_10X = arg0K0;
  if ((i_10X == (Snumber_of_channelsS))) {
    arg0K0 = (Snumber_of_channelsS);
    goto L2425;}
  else {
    *(new_vm_channels_9X + i_10X) = (*((Svm_channelsS) + i_10X));
    arg0K0 = (1 + i_10X);
    goto L2410;}}
 L2425: {
  i_11X = arg0K0;
  if ((i_11X == new_count_8X)) {
    free((Svm_channelsS));
    Svm_channelsS = new_vm_channels_9X;
    Snumber_of_channelsS = new_count_8X;
    return 1;}
  else {
    *(new_vm_channels_9X + i_11X) = 1;
    arg0K0 = (1 + i_11X);
    goto L2425;}}
}
static void enqueue_input_channelB(long index_12X, long status_13X)
{
  char * addr_18X;
  long x_17X;
  char * addr_16X;
  long val_15X;
  long channel_14X;
 {  channel_14X = *((Svm_channelsS) + index_12X);
  val_15X = ((status_13X)<<2);
  addr_16X = (((char *) (-3 + channel_14X))) + 16;
  S48_WRITE_BARRIER(channel_14X, addr_16X, val_15X);
  *((long *) addr_16X) = val_15X;
  if ((1 == (*((long *) ((((char *) (-3 + channel_14X))) + 12))))) {
    if ((1 == (Spending_input_channels_headS))) {
      Spending_input_channels_headS = channel_14X;
      Spending_input_channels_tailS = channel_14X;
      return;}
    else {
      x_17X = Spending_input_channels_tailS;
      addr_18X = (((char *) (-3 + x_17X))) + 12;
      S48_WRITE_BARRIER(x_17X, addr_18X, channel_14X);
      *((long *) addr_18X) = channel_14X;
      Spending_input_channels_tailS = channel_14X;
      return;}}
  else {
    return;}}
}
static void enqueue_output_channelB(long index_19X, long status_20X)
{
  char * addr_25X;
  long x_24X;
  char * addr_23X;
  long val_22X;
  long channel_21X;
 {  channel_21X = *((Svm_channelsS) + index_19X);
  val_22X = ((status_20X)<<2);
  addr_23X = (((char *) (-3 + channel_21X))) + 16;
  S48_WRITE_BARRIER(channel_21X, addr_23X, val_22X);
  *((long *) addr_23X) = val_22X;
  if ((1 == (*((long *) ((((char *) (-3 + channel_21X))) + 12))))) {
    if ((1 == (Spending_output_channels_headS))) {
      Spending_output_channels_headS = channel_21X;
      Spending_output_channels_tailS = channel_21X;
      return;}
    else {
      x_24X = Spending_output_channels_tailS;
      addr_25X = (((char *) (-3 + x_24X))) + 12;
      S48_WRITE_BARRIER(x_24X, addr_25X, channel_21X);
      *((long *) addr_25X) = channel_21X;
      Spending_output_channels_tailS = channel_21X;
      return;}}
  else {
    return;}}
}
static long close_channelB(long channel_26X)
{
  long arg0K0;
  char * addr_36X;
  long status_35X;
  long v_34X;
  long v_33X;
  long v_32X;
  long v_31X;
  long v_30X;
  long v_29X;
  long x_28X;
  long os_index_27X;
 {  os_index_27X = (((*((long *) ((((char *) (-3 + channel_26X))) + 8))))>>2);
  x_28X = *((long *) ((((char *) (-3 + channel_26X))) + 16));
  if ((5 == x_28X)) {
    v_29X = *((long *) (((char *) (-3 + channel_26X))));
    if ((4 == v_29X)) {
      goto L8295;}
    else {
      if ((12 == (*((long *) (((char *) (-3 + channel_26X))))))) {
        goto L8295;}
      else {
        v_30X = ps_abort_fd_op(os_index_27X);
        enqueue_output_channelB(os_index_27X, v_30X);
        goto L8307;}}}
  else {
    goto L8307;}}
 L8295: {
  v_31X = ps_abort_fd_op(os_index_27X);
  enqueue_input_channelB(os_index_27X, v_31X);
  goto L8307;}
 L8307: {
  v_32X = *((long *) (((char *) (-3 + channel_26X))));
  if ((4 == v_32X)) {
    goto L8322;}
  else {
    if ((12 == (*((long *) (((char *) (-3 + channel_26X))))))) {
      goto L8322;}
    else {
      v_33X = ps_close_fd(os_index_27X);
      arg0K0 = v_33X;
      goto L8329;}}}
 L8322: {
  v_34X = ps_close_fd(os_index_27X);
  arg0K0 = v_34X;
  goto L8329;}
 L8329: {
  status_35X = arg0K0;
  *((Svm_channelsS) + os_index_27X) = 1;
  addr_36X = ((char *) (-3 + channel_26X));
  S48_WRITE_BARRIER(channel_26X, addr_36X, 0);
  *((long *) addr_36X) = 0;
  return status_35X;}
}
static void push_continuationB(char * code_pointer_37X, long size_38X)
{
  long cont_41X;
  long template_40X;
  long pc_39X;
 {  pc_39X = (((code_pointer_37X - (((char *) (-3 + (*((long *) (((char *) (-3 + (StemplateS)))))))))))<<2);
  template_40X = StemplateS;
  SstackS = ((SstackS) + -20);
  *((long *) ((SstackS) + 4)) = (128 | (4138 + (((size_38X)<<10))));
  cont_41X = 3 + (((long) ((SstackS) + 8)));
  *((long *) ((((char *) (-3 + cont_41X))) + 4)) = pc_39X;
  *((long *) ((((char *) (-3 + cont_41X))) + 8)) = template_40X;
  *((long *) ((((char *) (-3 + cont_41X))) + 12)) = (SenvS);
  *((long *) (((char *) (-3 + cont_41X)))) = (ScontS);
  ScontS = cont_41X;
  return;}
}
static long enter_string(char *string_42X, long key_43X)
{
  long arg0K0;
  long i_48X;
  long string_47X;
  char * addr_46X;
  long len_45X;
  long z_44X;
 {  z_44X = strlen((char *) string_42X);
  len_45X = 1 + z_44X;
  addr_46X = ALLOCATE_SPACE(17, (4 + len_45X));
  *((long *) addr_46X) = (70 + (((len_45X)<<8)));
  string_47X = 3 + (((long) (addr_46X + 4)));
  *((unsigned char *) ((((char *) (-3 + string_47X))) + z_44X)) = 0;
  arg0K0 = 0;
  goto L10165;}
 L10165: {
  i_48X = arg0K0;
  if ((i_48X < z_44X)) {
    *((unsigned char *) ((((char *) (-3 + string_47X))) + i_48X)) = ((*(string_42X + i_48X)));
    arg0K0 = (1 + i_48X);
    goto L10165;}
  else {
    return string_47X;}}
}
static char HcleanerB2200(long table_49X)
{
  long arg0K1;
  long arg0K0;
  long v_60X;
  char * addr_59X;
  long new_foo_58X;
  char v_57X;
  char * addr_56X;
  long okay_55X;
  long foo_54X;
  long foo_53X;
  char temp_52X;
  long i_51X;
  long table_50X;
 {  table_50X = s48_trace_value(table_49X);
  arg0K0 = 0;
  goto L10355;}
 L10355: {
  i_51X = arg0K0;
  temp_52X = 1024 == i_51X;
  if (temp_52X) {
    return temp_52X;}
  else {
    foo_53X = *((long *) ((((char *) (-3 + table_50X))) + (((i_51X)<<2))));
    arg0K0 = foo_53X;
    arg0K1 = 1;
    goto L10375;}}
 L10375: {
  foo_54X = arg0K0;
  okay_55X = arg0K1;
  if ((1 == foo_54X)) {
    addr_56X = (((char *) (-3 + table_50X))) + (((i_51X)<<2));
    S48_WRITE_BARRIER(table_50X, addr_56X, okay_55X);
    *((long *) addr_56X) = okay_55X;
    arg0K0 = (1 + i_51X);
    goto L10355;}
  else {
    v_57X = s48_extantP(foo_54X);
    if (v_57X) {
      new_foo_58X = s48_trace_value(foo_54X);
      addr_59X = (((char *) (-3 + new_foo_58X))) + 4;
      S48_WRITE_BARRIER(new_foo_58X, addr_59X, okay_55X);
      *((long *) addr_59X) = okay_55X;
      arg0K0 = new_foo_58X;
      goto L10380;}
    else {
      arg0K0 = okay_55X;
      goto L10380;}}}
 L10380: {
  v_60X = arg0K0;
  arg0K0 = (*((long *) ((((char *) (-3 + foo_54X))) + 4)));
  arg0K1 = v_60X;
  goto L10375;}
}
static void push_exception_continuationB(long exception_61X, long instruction_size_62X)
{
  char * arg1K0;
  long arg0K1;
  long arg0K0;
  long v_67X;
  long i_66X;
  char * p_65X;
  long tem_64X;
  long opcode_63X;
 {  opcode_63X = *((unsigned char *) (Scode_pointerS));
  *((long *) (SstackS)) = (((instruction_size_62X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((exception_61X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (StemplateS);
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((((Scode_pointerS) - (((char *) (-3 + (*((long *) (((char *) (-3 + (StemplateS)))))))))))<<2));
  SstackS = ((SstackS) + -4);
  tem_64X = Sexception_templateS;
  StemplateS = tem_64X;
  Scode_pointerS = (((char *) (-3 + (*((long *) (((char *) (-3 + tem_64X))))))));
  arg1K0 = ((SstackS) + 4);
  arg0K1 = 0;
  goto L10589;}
 L10589: {
  p_65X = arg1K0;
  i_66X = arg0K1;
  if ((2 == (3 & (*((long *) p_65X))))) {
    if ((26 == (*((long *) p_65X)))) {
      arg0K0 = (-1 + i_66X);
      goto L10530;}
    else {
      arg0K0 = i_66X;
      goto L10530;}}
  else {
    arg1K0 = (p_65X + 4);
    arg0K1 = (1 + i_66X);
    goto L10589;}}
 L10530: {
  v_67X = arg0K0;
  push_continuationB((Scode_pointerS), v_67X);
  *((long *) (SstackS)) = (((opcode_63X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((exception_61X)<<2));
  SstackS = ((SstackS) + -4);
  return;}
}
static long make_registered_channel(long mode_68X, long id_69X, long os_index_70X, long key_71X, long *TT0)
{
  long x_75X;
  char * addr_74X;
  char x_73X;
  char temp_72X;
 {  temp_72X = os_index_70X < (Snumber_of_channelsS);
  if (temp_72X) {
    goto L11458;}
  else {
    x_73X = add_more_channels(os_index_70X);
    if (x_73X) {
      goto L11458;}
    else {
      *TT0 = 9;
      return 1;}}}
 L11458: {
  if ((1 == (*((Svm_channelsS) + os_index_70X)))) {
    addr_74X = ALLOCATE_SPACE(6, 24);
    *((long *) addr_74X) = 5146;
    x_75X = 3 + (((long) (addr_74X + 4)));
    *((long *) (((char *) (-3 + x_75X)))) = (((mode_68X)<<2));
    *((long *) ((((char *) (-3 + x_75X))) + 4)) = id_69X;
    *((long *) ((((char *) (-3 + x_75X))) + 8)) = (((os_index_70X)<<2));
    *((long *) ((((char *) (-3 + x_75X))) + 12)) = 1;
    *((long *) ((((char *) (-3 + x_75X))) + 16)) = 1;
    *((Svm_channelsS) + os_index_70X) = x_75X;
    *TT0 = 9;
    return x_75X;}
  else {
    *TT0 = 11;
    return 1;}}
}
static long channel_close_error(long status_76X, long index_77X, long id_78X)
{
  long v_79X;
 {  ps_write_string("Error: ", (stderr));
  ps_write_string((ps_error_string(status_76X)), (stderr));
  { long ignoreXX;
  PS_WRITE_CHAR(10, (stderr), ignoreXX) }
  ps_write_string(" while closing port ", (stderr));
  if ((3 == (3 & id_78X))) {
    if ((17 == (31 & ((((*((long *) ((((char *) (-3 + id_78X))) + -4))))>>2))))) {
      ps_write_string((((char *)(((char *) (-3 + id_78X))))), (stderr));
      goto L12104;}
    else {
      goto L12098;}}
  else {
    goto L12098;}}
 L12104: {
  PS_WRITE_CHAR(10, (stderr), v_79X)
  return v_79X;}
 L12098: {
  ps_write_integer((((index_77X)>>2)), (stderr));
  goto L12104;}
}
static void HtopD9001(void)
{
  char * arg1K0;
  long arg0K1;
  long arg0K0;
  long env_123X;
  long env_122X;
  long last_env_121X;
  long cont_120X;
  long env_119X;
  long v_118X;
  long env_117X;
  long p_116X;
  long arg_count_115X;
  long i_114X;
  char * p_113X;
  char * addr_112X;
  long val_111X;
  long unused_110X;
  char * a_109X;
  char x_108X;
  long pair_107X;
  long v_106X;
  long v_105X;
  long v_104X;
  long v_103X;
  long env_102X;
  long alist_101X;
  long x2_100X;
  char * cell_99X;
  long i_98X;
  long x2_97X;
  char * cell_96X;
  long v_95X;
  long v_94X;
  long v_93X;
  long v_92X;
  long v_91X;
  long v_90X;
  long v_89X;
  long v_88X;
  long v_87X;
  long v_86X;
  long v_85X;
  long v_84X;
  long v_83X;
  char * frame_82X;
  long length_81X;
  char * frame_80X;
 {  arg1K0 = (Sexternal_root_stackS);
  goto L6478;}
 L6478: {
  frame_80X = arg1K0;
  if ((frame_80X == NULL)) {
    arg1K0 = (Spermanent_external_rootsS);
    goto L6504;}
  else {
    length_81X = *((long *) frame_80X);
    arg0K0 = 0;
    goto L6486;}}
 L6504: {
  frame_82X = arg1K0;
  if ((frame_82X == NULL)) {
    s48_initializing_gc_root();
    v_83X = s48_trace_value((Simported_bindingsS));
    Simported_bindingsS = v_83X;
    v_84X = s48_trace_value((Sexported_bindingsS));
    Sexported_bindingsS = v_84X;
    Ssaved_pcS = (((((Scode_pointerS) - (((char *) (-3 + (*((long *) (((char *) (-3 + (StemplateS)))))))))))<<2));
    v_85X = s48_trace_value((StemplateS));
    StemplateS = v_85X;
    v_86X = s48_trace_value((SvalS));
    SvalS = v_86X;
    v_87X = s48_trace_value((Scurrent_threadS));
    Scurrent_threadS = v_87X;
    v_88X = s48_trace_value((Ssession_dataS));
    Ssession_dataS = v_88X;
    v_89X = s48_trace_value((Sexception_handlersS));
    Sexception_handlersS = v_89X;
    v_90X = s48_trace_value((Sexception_templateS));
    Sexception_templateS = v_90X;
    v_91X = s48_trace_value((Sinterrupt_handlersS));
    Sinterrupt_handlersS = v_91X;
    v_92X = s48_trace_value((Sinterrupt_templateS));
    Sinterrupt_templateS = v_92X;
    v_93X = s48_trace_value((Sinterrupted_templateS));
    Sinterrupted_templateS = v_93X;
    v_94X = s48_trace_value((Sfinalize_theseS));
    Sfinalize_theseS = v_94X;
    v_95X = s48_trace_value((Sos_signal_listS));
    Sos_signal_listS = v_95X;
    arg0K0 = (Sfinalizer_alistS);
    goto L5381;}
  else {
    cell_96X = ((char *) (*((long *) (frame_82X + 4))));
    x2_97X = s48_trace_value((*((long *) cell_96X)));
    *((long *) cell_96X) = x2_97X;
    arg1K0 = (((char *) (*((long *) frame_82X))));
    goto L6504;}}
 L6486: {
  i_98X = arg0K0;
  if ((i_98X == length_81X)) {
    arg1K0 = (((char *) (*((long *) (frame_80X + 4)))));
    goto L6478;}
  else {
    cell_99X = ((char *) (*((long *) (frame_80X + (8 + (((i_98X)<<2)))))));
    x2_100X = s48_trace_value((*((long *) cell_99X)));
    *((long *) cell_99X) = x2_100X;
    arg0K0 = (1 + i_98X);
    goto L6486;}}
 L5381: {
  alist_101X = arg0K0;
  if ((25 == alist_101X)) {
    env_102X = s48_trace_value((SenvS));
    SenvS = env_102X;
    v_103X = s48_trace_value((Spending_input_channels_headS));
    Spending_input_channels_headS = v_103X;
    v_104X = s48_trace_value((Spending_input_channels_tailS));
    Spending_input_channels_tailS = v_104X;
    v_105X = s48_trace_value((Spending_output_channels_headS));
    Spending_output_channels_headS = v_105X;
    v_106X = s48_trace_value((Spending_output_channels_tailS));
    Spending_output_channels_tailS = v_106X;
    if ((Sstack_warningPS)) {
      arg1K0 = (Sstack_beginS);
      goto L6106;}
    else {
      goto L6139;}}
  else {
    pair_107X = *((long *) (((char *) (-3 + alist_101X))));
    x_108X = s48_extantP((*((long *) (((char *) (-3 + pair_107X))))));
    if (x_108X) {
      goto L5406;}
    else {
      s48_trace_stob_contentsB((*((long *) (((char *) (-3 + pair_107X))))));
      goto L5406;}}}
 L6106: {
  a_109X = arg1K0;
  if ((252645135 == (*((long *) a_109X)))) {
    arg1K0 = (a_109X + 4);
    goto L6106;}
  else {
    unused_110X = (((a_109X - (Sstack_beginS)))>>2);
    if ((unused_110X < 30)) {
      { long ignoreXX;
      PS_WRITE_CHAR(10, (stderr), ignoreXX) }
      ps_write_string("[Alert: stack overconsumption (", (stderr));
      ps_write_integer(unused_110X, (stderr));
      ps_write_string("); please inform the Scheme 48 implementors]", (stderr));
      { long ignoreXX;
      PS_WRITE_CHAR(10, (stderr), ignoreXX) }
      Sstack_warningPS = 0;
      goto L6139;}
    else {
      goto L6139;}}}
 L6139: {
  arg1K0 = ((SstackS) + 4);
  arg0K1 = 0;
  goto L6204;}
 L5406: {
  val_111X = s48_trace_value((*((long *) ((((char *) (-3 + pair_107X))) + 4))));
  addr_112X = (((char *) (-3 + pair_107X))) + 4;
  S48_WRITE_BARRIER(pair_107X, addr_112X, val_111X);
  *((long *) addr_112X) = val_111X;
  arg0K0 = (*((long *) ((((char *) (-3 + alist_101X))) + 4)));
  goto L5381;}
 L6204: {
  p_113X = arg1K0;
  i_114X = arg0K1;
  if ((2 == (3 & (*((long *) p_113X))))) {
    if ((26 == (*((long *) p_113X)))) {
      arg0K0 = (-1 + i_114X);
      goto L6141;}
    else {
      arg0K0 = i_114X;
      goto L6141;}}
  else {
    arg1K0 = (p_113X + 4);
    arg0K1 = (1 + i_114X);
    goto L6204;}}
 L6141: {
  arg_count_115X = arg0K0;
  s48_trace_locationsB(((SstackS) + 4), ((SstackS) + (4 + (((arg_count_115X)<<2)))));
  p_116X = SenvS;
  if ((3 == (3 & p_116X))) {
    if ((p_116X < (((long) (Sstack_beginS))))) {
      goto L6157;}
    else {
      if (((((long) (Sstack_endS))) < p_116X)) {
        goto L6157;}
      else {
        env_117X = SenvS;
        arg0K0 = env_117X;
        goto L6317;}}}
  else {
    goto L6157;}}
 L6157: {
  v_118X = s48_trace_value((SenvS));
  SenvS = v_118X;
  goto L6162;}
 L6317: {
  env_119X = arg0K0;
  if ((3 == (3 & env_119X))) {
    if ((env_119X < (((long) (Sstack_beginS))))) {
      goto L6162;}
    else {
      if (((((long) (Sstack_endS))) < env_119X)) {
        goto L6162;}
      else {
        s48_trace_stob_contentsB(env_119X);
        arg0K0 = (*((long *) (((char *) (-3 + env_119X)))));
        goto L6317;}}}
  else {
    goto L6162;}}
 L6162: {
  arg0K0 = (ScontS);
  arg0K1 = 0;
  goto L6167;}
 L6167: {
  cont_120X = arg0K0;
  last_env_121X = arg0K1;
  env_122X = *((long *) ((((char *) (-3 + cont_120X))) + 12));
  s48_trace_stob_contentsB(cont_120X);
  if ((env_122X == last_env_121X)) {
    goto L6185;}
  else {
    arg0K0 = env_122X;
    goto L6272;}}
 L6185: {
  if ((cont_120X == (Sbottom_of_stackS))) {
    return;}
  else {
    arg0K0 = (*((long *) (((char *) (-3 + cont_120X)))));
    arg0K1 = env_122X;
    goto L6167;}}
 L6272: {
  env_123X = arg0K0;
  if ((3 == (3 & env_123X))) {
    if ((env_123X < (((long) (Sstack_beginS))))) {
      goto L6185;}
    else {
      if (((((long) (Sstack_endS))) < env_123X)) {
        goto L6185;}
      else {
        s48_trace_stob_contentsB(env_123X);
        arg0K0 = (*((long *) (((char *) (-3 + env_123X)))));
        goto L6272;}}}
  else {
    goto L6185;}}
}
static long Hlookup2142(long table_124X, long string_125X, long key_126X)
{
  long arg0K1;
  long arg0K0;
  long len_137X;
  long s2_136X;
  char * addr_135X;
  long x_134X;
  char * addr_133X;
  long foo_132X;
  long bucket_131X;
  long index_130X;
  long h_129X;
  long i_128X;
  long n_127X;
 {  n_127X = -1 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + string_125X))) + -4))))>>8));
  arg0K0 = 0;
  arg0K1 = 0;
  goto L13725;}
 L13725: {
  i_128X = arg0K0;
  h_129X = arg0K1;
  if ((i_128X < n_127X)) {
    arg0K0 = (1 + i_128X);
    arg0K1 = (h_129X + (((*((unsigned char *) ((((char *) (-3 + string_125X))) + i_128X))))));
    goto L13725;}
  else {
    index_130X = 1023 & h_129X;
    bucket_131X = *((long *) ((((char *) (-3 + table_124X))) + (((index_130X)<<2))));
    arg0K0 = bucket_131X;
    goto L13693;}}
 L13693: {
  foo_132X = arg0K0;
  if ((1 == foo_132X)) {
    addr_133X = ALLOCATE_SPACE(14, 20);
    *((long *) addr_133X) = 4154;
    x_134X = 3 + (((long) (addr_133X + 4)));
    *((long *) (((char *) (-3 + x_134X)))) = string_125X;
    *((long *) ((((char *) (-3 + x_134X))) + 4)) = 1;
    *((long *) ((((char *) (-3 + x_134X))) + 8)) = 13;
    *((long *) ((((char *) (-3 + x_134X))) + 12)) = bucket_131X;
    addr_135X = (((char *) (-3 + table_124X))) + (((index_130X)<<2));
    S48_WRITE_BARRIER(table_124X, addr_135X, x_134X);
    *((long *) addr_135X) = x_134X;
    return x_134X;}
  else {
    s2_136X = *((long *) (((char *) (-3 + foo_132X))));
    len_137X = (long)(((unsigned long)(*((long *) ((((char *) (-3 + string_125X))) + -4))))>>8);
    if ((len_137X == ((long)(((unsigned long)(*((long *) ((((char *) (-3 + s2_136X))) + -4))))>>8)))) {
      if (((!memcmp((void *)(((char *) (-3 + s2_136X))), (void *)(((char *) (-3 + string_125X))),len_137X)))) {
        return foo_132X;}
      else {
        goto L13709;}}
    else {
      goto L13709;}}}
 L13709: {
  arg0K0 = (*((long *) ((((char *) (-3 + foo_132X))) + 12)));
  goto L13693;}
}
static long Hlookup2123(long table_138X, long string_139X, long key_140X)
{
  long arg0K1;
  long arg0K0;
  long len_151X;
  long s2_150X;
  char * addr_149X;
  long x_148X;
  char * addr_147X;
  long foo_146X;
  long bucket_145X;
  long index_144X;
  long h_143X;
  long i_142X;
  long n_141X;
 {  n_141X = -1 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + string_139X))) + -4))))>>8));
  arg0K0 = 0;
  arg0K1 = 0;
  goto L13850;}
 L13850: {
  i_142X = arg0K0;
  h_143X = arg0K1;
  if ((i_142X < n_141X)) {
    arg0K0 = (1 + i_142X);
    arg0K1 = (h_143X + (((*((unsigned char *) ((((char *) (-3 + string_139X))) + i_142X))))));
    goto L13850;}
  else {
    index_144X = 1023 & h_143X;
    bucket_145X = *((long *) ((((char *) (-3 + table_138X))) + (((index_144X)<<2))));
    arg0K0 = bucket_145X;
    goto L13818;}}
 L13818: {
  foo_146X = arg0K0;
  if ((1 == foo_146X)) {
    addr_147X = ALLOCATE_SPACE(14, 20);
    *((long *) addr_147X) = 4154;
    x_148X = 3 + (((long) (addr_147X + 4)));
    *((long *) (((char *) (-3 + x_148X)))) = string_139X;
    *((long *) ((((char *) (-3 + x_148X))) + 4)) = 5;
    *((long *) ((((char *) (-3 + x_148X))) + 8)) = 13;
    *((long *) ((((char *) (-3 + x_148X))) + 12)) = bucket_145X;
    addr_149X = (((char *) (-3 + table_138X))) + (((index_144X)<<2));
    S48_WRITE_BARRIER(table_138X, addr_149X, x_148X);
    *((long *) addr_149X) = x_148X;
    return x_148X;}
  else {
    s2_150X = *((long *) (((char *) (-3 + foo_146X))));
    len_151X = (long)(((unsigned long)(*((long *) ((((char *) (-3 + string_139X))) + -4))))>>8);
    if ((len_151X == ((long)(((unsigned long)(*((long *) ((((char *) (-3 + s2_150X))) + -4))))>>8)))) {
      if (((!memcmp((void *)(((char *) (-3 + s2_150X))), (void *)(((char *) (-3 + string_139X))),len_151X)))) {
        return foo_146X;}
      else {
        goto L13834;}}
    else {
      goto L13834;}}}
 L13834: {
  arg0K0 = (*((long *) ((((char *) (-3 + foo_146X))) + 12)));
  goto L13818;}
}
static void HtopD8994(void)
{
  long arg0K2;
  long arg0K1;
  long arg0K0;
  long status_178X;
  long id_177X;
  long new_176X;
  long id_175X;
  long header_174X;
  long channel_173X;
  long i_172X;
  char * addr_171X;
  long l_170X;
  long v_169X;
  char * addr_168X;
  char * addr_167X;
  char * addr_166X;
  char * addr_165X;
  long val_164X;
  char tracedP_163X;
  long next_162X;
  long thing_161X;
  long pair_160X;
  long alist_159X;
  long l2_158X;
  long goners_157X;
  long okay_156X;
  long alist_155X;
  long pc_154X;
  long tem_153X;
  long new_152X;
 {  new_152X = s48_trace_value((Sthe_symbol_tableS));
  HcleanerB2200(new_152X);
  Sthe_symbol_tableS = new_152X;
  tem_153X = StemplateS;
  pc_154X = Ssaved_pcS;
  StemplateS = tem_153X;
  Scode_pointerS = ((((char *) (-3 + (*((long *) (((char *) (-3 + tem_153X)))))))) + (((pc_154X)>>2)));
  arg0K0 = (Sfinalizer_alistS);
  arg0K1 = 25;
  arg0K2 = 25;
  goto L7945;}
 L7945: {
  alist_155X = arg0K0;
  okay_156X = arg0K1;
  goners_157X = arg0K2;
  if ((25 == alist_155X)) {
    Sfinalizer_alistS = okay_156X;
    l2_158X = Sfinalize_theseS;
    if ((25 == goners_157X)) {
      arg0K0 = l2_158X;
      goto L7953;}
    else {
      arg0K0 = goners_157X;
      goto L8006;}}
  else {
    alist_159X = s48_trace_value(alist_155X);
    pair_160X = s48_trace_value((*((long *) (((char *) (-3 + alist_159X))))));
    thing_161X = *((long *) (((char *) (-3 + pair_160X))));
    next_162X = *((long *) ((((char *) (-3 + alist_159X))) + 4));
    tracedP_163X = s48_extantP(thing_161X);
    val_164X = s48_trace_value(thing_161X);
    addr_165X = ((char *) (-3 + pair_160X));
    S48_WRITE_BARRIER(pair_160X, addr_165X, val_164X);
    *((long *) addr_165X) = val_164X;
    addr_166X = ((char *) (-3 + alist_159X));
    S48_WRITE_BARRIER(alist_159X, addr_166X, pair_160X);
    *((long *) addr_166X) = pair_160X;
    if (tracedP_163X) {
      addr_167X = (((char *) (-3 + alist_159X))) + 4;
      S48_WRITE_BARRIER(alist_159X, addr_167X, okay_156X);
      *((long *) addr_167X) = okay_156X;
      arg0K0 = next_162X;
      arg0K1 = alist_159X;
      arg0K2 = goners_157X;
      goto L7945;}
    else {
      addr_168X = (((char *) (-3 + alist_159X))) + 4;
      S48_WRITE_BARRIER(alist_159X, addr_168X, goners_157X);
      *((long *) addr_168X) = goners_157X;
      arg0K0 = next_162X;
      arg0K1 = okay_156X;
      arg0K2 = alist_159X;
      goto L7945;}}}
 L7953: {
  v_169X = arg0K0;
  Sfinalize_theseS = v_169X;
  arg0K0 = 0;
  goto L13157;}
 L8006: {
  l_170X = arg0K0;
  if ((25 == (*((long *) ((((char *) (-3 + l_170X))) + 4))))) {
    addr_171X = (((char *) (-3 + l_170X))) + 4;
    S48_WRITE_BARRIER(l_170X, addr_171X, l2_158X);
    *((long *) addr_171X) = l2_158X;
    arg0K0 = goners_157X;
    goto L7953;}
  else {
    arg0K0 = (*((long *) ((((char *) (-3 + l_170X))) + 4)));
    goto L8006;}}
 L13157: {
  i_172X = arg0K0;
  if ((i_172X == (Snumber_of_channelsS))) {
    Spending_interruptsS = (4 | (Spending_interruptsS));
    if ((0 == ((Spending_interruptsS) & (Senabled_interruptsS)))) {
      s48_Spending_interruptPS = 0;
      if ((s48_Spending_eventsPS)) {
        s48_Spending_interruptPS = 1;
        return;}
      else {
        return;}}
    else {
      s48_Spending_interruptPS = 1;
      return;}}
  else {
    channel_173X = *((Svm_channelsS) + i_172X);
    if ((1 == channel_173X)) {
      goto L13193;}
    else {
      header_174X = *((long *) ((((char *) (-3 + channel_173X))) + -4));
      if ((3 == (3 & header_174X))) {
        arg0K0 = header_174X;
        goto L13186;}
      else {
        if ((0 == (*((long *) (((char *) (-3 + channel_173X))))))) {
          arg0K0 = 1;
          goto L13186;}
        else {
          id_175X = *((long *) ((((char *) (-3 + channel_173X))) + 4));
          if ((0 == (3 & id_175X))) {
            arg0K0 = id_175X;
            goto L12869;}
          else {
            if ((3 == (3 & (*((long *) ((((char *) (-3 + id_175X))) + -4)))))) {
              arg0K0 = (*((long *) ((((char *) (-3 + id_175X))) + -4)));
              goto L12869;}
            else {
              arg0K0 = id_175X;
              goto L12869;}}}}}}}
 L13193: {
  arg0K0 = (1 + i_172X);
  goto L13157;}
 L13186: {
  new_176X = arg0K0;
  *((Svm_channelsS) + i_172X) = new_176X;
  goto L13193;}
 L12869: {
  id_177X = arg0K0;
  status_178X = close_channelB(channel_173X);
  if ((status_178X == NO_ERRORS)) {
    goto L12884;}
  else {
    channel_close_error(status_178X, (*((long *) ((((char *) (-3 + channel_173X))) + 8))), id_177X);
    goto L12884;}}
 L12884: {
  ps_write_string("Channel closed: ", (stderr));
  if ((0 == (3 & id_177X))) {
    ps_write_integer((((id_177X)>>2)), (stderr));
    goto L12902;}
  else {
    ps_write_string((((char *)(((char *) (-3 + id_177X))))), (stderr));
    goto L12902;}}
 L12902: {
  ps_write_string(" ", (stderr));
  ps_write_integer(((((*((long *) ((((char *) (-3 + channel_173X))) + 8))))>>2)), (stderr));
  { long ignoreXX;
  PS_WRITE_CHAR(10, (stderr), ignoreXX) }
  arg0K0 = 1;
  goto L13186;}
}
static long collect_saving_temps(long value0_179X, long value1_180X, long *TT0)
{
  long value0_182X;
  long value1_181X;
 {  s48_begin_collection();
  (*(Sgc_root_procS))();
  value1_181X = s48_trace_value(value1_180X);
  value0_182X = s48_trace_value(value0_179X);
  s48_do_gc();
  (*(Spost_gc_cleanupS))();
  s48_end_collection();
  *TT0 = value1_181X;
  return value0_182X;}
}
void s48_set_extension_valueB(long value_183X)
{

 {  s48_Sextension_valueS = value_183X;
  return;}
}
void s48_note_event(void)
{

 {  s48_Spending_eventsPS = 1;
  s48_Spending_interruptPS = 1;
  return;}
}
long s48_exported_bindings(void)
{

 {  return (Sexported_bindingsS);}
}
char * s48_set_gc_roots_baseB(char * *TT0)
{
  char * old_base_184X;
 {  old_base_184X = Sexternal_root_stack_baseS;
  Sexternal_root_stack_baseS = (Sexternal_root_stackS);
  *TT0 = (Sexternal_root_stackS);
  return old_base_184X;}
}
char s48_release_gc_roots_baseB(char * old_base_185X, char * old_stack_186X)
{
  char okayP_187X;
 {  okayP_187X = (Sexternal_root_stackS) == (Sexternal_root_stack_baseS);
  Sexternal_root_stack_baseS = old_base_185X;
  Sexternal_root_stackS = old_stack_186X;
  return okayP_187X;}
}
void s48_disable_interruptsB(void)
{

 {  s48_Spending_interruptPS = 0;
  Senabled_interruptsS = 0;
  return;}
}
void s48_push_gc_rootsB(char * frame_188X, long n_189X)
{

 {  *((long *) frame_188X) = n_189X;
  *((long *) (frame_188X + 4)) = (((long) (Sexternal_root_stackS)));
  Sexternal_root_stackS = frame_188X;
  return;}
}
void s48_register_gc_rootB(char * loc_addr_190X)
{
  char * frame_191X;
 {  frame_191X = (char *)malloc(8);
  if ((frame_191X == NULL)) {
    ps_error("out of memory registering a global root", 0);
    goto L3195;}
  else {
    goto L3195;}}
 L3195: {
  *((long *) frame_191X) = (((long) (Spermanent_external_rootsS)));
  *((long *) (frame_191X + 4)) = (((long) loc_addr_190X));
  Spermanent_external_rootsS = frame_191X;
  return;}
}
void s48_stack_setB(long x_192X, long value_193X)
{

 {  *((long *) ((SstackS) + (4 + (((x_192X)<<2))))) = value_193X;
  return;}
}
long s48_stack_ref(long i_194X)
{

 {  return (*((long *) ((SstackS) + (4 + (((i_194X)<<2))))));}
}
void s48_push(long x_195X)
{

 {  *((long *) (SstackS)) = x_195X;
  SstackS = ((SstackS) + -4);
  return;}
}
long s48_resetup_external_exception(long new_why_196X, long additional_nargs_197X)
{
  long old_why_199X;
  long old_nargs_198X;
 {  old_nargs_198X = Sexternal_exception_nargsS;
  old_why_199X = *((long *) ((SstackS) + (4 + (((old_nargs_198X)<<2)))));
  *((long *) ((SstackS) + (4 + (((old_nargs_198X)<<2))))) = (((new_why_196X)<<2));
  Sexternal_exception_nargsS = (old_nargs_198X + additional_nargs_197X);
  return old_why_199X;}
}
char s48_pop_gc_rootsB(void)
{

 {  if (((Sexternal_root_stackS) == (Sexternal_root_stack_baseS))) {
    return 0;}
  else {
    Sexternal_root_stackS = (((char *) (*((long *) ((Sexternal_root_stackS) + 4)))));
    return 1;}}
}
void s48_enable_interruptsB(void)
{

 {  Senabled_interruptsS = -1;
  if ((0 == ((Spending_interruptsS) & (Senabled_interruptsS)))) {
    s48_Spending_interruptPS = 0;
    if ((s48_Spending_eventsPS)) {
      s48_Spending_interruptPS = 1;
      return;}
    else {
      return;}}
  else {
    s48_Spending_interruptPS = 1;
    return;}}
}
void s48_mark_traced_channels_closedB(void)
{
  long arg0K0;
  char * addr_205X;
  char * addr_204X;
  long descriptor_203X;
  long header_202X;
  long channel_201X;
  long i_200X;
 {  arg0K0 = 0;
  goto L6555;}
 L6555: {
  i_200X = arg0K0;
  if ((i_200X == (Snumber_of_channelsS))) {
    return;}
  else {
    channel_201X = *((Svm_channelsS) + i_200X);
    if ((1 == channel_201X)) {
      goto L6615;}
    else {
      header_202X = *((long *) ((((char *) (-3 + channel_201X))) + -4));
      if ((3 == (3 & header_202X))) {
        ps_write_string("Channel closed in dumped image: ", (stderr));
        descriptor_203X = *((long *) ((((char *) (-3 + channel_201X))) + 4));
        if ((0 == (3 & descriptor_203X))) {
          ps_write_integer(((((*((long *) ((((char *) (-3 + channel_201X))) + 4))))>>2)), (stderr));
          goto L6601;}
        else {
          ps_write_string((((char *)(((char *) (-3 + (*((long *) ((((char *) (-3 + channel_201X))) + 4)))))))), (stderr));
          goto L6601;}}
      else {
        goto L6615;}}}}
 L6615: {
  arg0K0 = (1 + i_200X);
  goto L6555;}
 L6601: {
  { long ignoreXX;
  PS_WRITE_CHAR(10, (stderr), ignoreXX) }
  addr_204X = ((char *) (-3 + header_202X));
  S48_WRITE_BARRIER(header_202X, addr_204X, 0);
  *((long *) addr_204X) = 0;
  addr_205X = (((char *) (-3 + header_202X))) + 8;
  S48_WRITE_BARRIER(header_202X, addr_205X, -4);
  *((long *) addr_205X) = -4;
  goto L6615;}
}
void s48_set_os_signals(long signal_list_206X)
{
  long arg0K0;
  char * addr_210X;
  long l_209X;
  long v_208X;
  long l1_207X;
 {  l1_207X = Sos_signal_listS;
  if ((25 == l1_207X)) {
    arg0K0 = signal_list_206X;
    goto L8074;}
  else {
    arg0K0 = l1_207X;
    goto L8085;}}
 L8074: {
  v_208X = arg0K0;
  Sos_signal_listS = v_208X;
  return;}
 L8085: {
  l_209X = arg0K0;
  if ((25 == (*((long *) ((((char *) (-3 + l_209X))) + 4))))) {
    addr_210X = (((char *) (-3 + l_209X))) + 4;
    S48_WRITE_BARRIER(l_209X, addr_210X, signal_list_206X);
    *((long *) addr_210X) = signal_list_206X;
    arg0K0 = l1_207X;
    goto L8074;}
  else {
    arg0K0 = (*((long *) ((((char *) (-3 + l_209X))) + 4)));
    goto L8085;}}
}
long s48_set_channel_os_index(long channel_211X, long os_index_212X)
{
  char * addr_221X;
  long val_220X;
  long v_219X;
  long v_218X;
  long v_217X;
  long x_216X;
  long old_index_215X;
  char x_214X;
  char temp_213X;
 {  temp_213X = os_index_212X < (Snumber_of_channelsS);
  if (temp_213X) {
    goto L8169;}
  else {
    x_214X = add_more_channels(os_index_212X);
    if (x_214X) {
      goto L8169;}
    else {
      return 36;}}}
 L8169: {
  if ((1 == (*((Svm_channelsS) + os_index_212X)))) {
    old_index_215X = (((*((long *) ((((char *) (-3 + channel_211X))) + 8))))>>2);
    x_216X = *((long *) ((((char *) (-3 + channel_211X))) + 16));
    if ((5 == x_216X)) {
      v_217X = *((long *) (((char *) (-3 + channel_211X))));
      if ((4 == v_217X)) {
        goto L8143;}
      else {
        if ((12 == (*((long *) (((char *) (-3 + channel_211X))))))) {
          goto L8143;}
        else {
          v_218X = ps_abort_fd_op(old_index_215X);
          enqueue_output_channelB(old_index_215X, v_218X);
          goto L8155;}}}
    else {
      goto L8155;}}
  else {
    return 44;}}
 L8143: {
  v_219X = ps_abort_fd_op(old_index_215X);
  enqueue_input_channelB(old_index_215X, v_219X);
  goto L8155;}
 L8155: {
  *((Svm_channelsS) + old_index_215X) = 1;
  *((Svm_channelsS) + os_index_212X) = channel_211X;
  val_220X = ((os_index_212X)<<2);
  addr_221X = (((char *) (-3 + channel_211X))) + 8;
  S48_WRITE_BARRIER(channel_211X, addr_221X, val_220X);
  *((long *) addr_221X) = val_220X;
  return 5;}
}
long s48_cleaned_imported_bindings(void)
{
  long arg0K1;
  long arg0K0;
  long v_235X;
  char * addr_234X;
  long entry_233X;
  char * addr_232X;
  long new_foo_231X;
  char v_230X;
  char * addr_229X;
  long okay_228X;
  long foo_227X;
  long i_226X;
  long foo_225X;
  long i_224X;
  long table_223X;
  long table_222X;
 {  table_222X = s48_trace_value((Simported_bindingsS));
  table_223X = s48_trace_value(table_222X);
  arg0K0 = 0;
  goto L9205;}
 L9205: {
  i_224X = arg0K0;
  if ((1024 == i_224X)) {
    arg0K0 = 0;
    goto L10306;}
  else {
    foo_225X = *((long *) ((((char *) (-3 + table_223X))) + (((i_224X)<<2))));
    arg0K0 = foo_225X;
    arg0K1 = 1;
    goto L9225;}}
 L10306: {
  i_226X = arg0K0;
  if ((1024 == i_226X)) {
    return table_222X;}
  else {
    arg0K0 = (*((long *) ((((char *) (-3 + table_222X))) + (((i_226X)<<2)))));
    goto L10314;}}
 L9225: {
  foo_227X = arg0K0;
  okay_228X = arg0K1;
  if ((1 == foo_227X)) {
    addr_229X = (((char *) (-3 + table_223X))) + (((i_224X)<<2));
    S48_WRITE_BARRIER(table_223X, addr_229X, okay_228X);
    *((long *) addr_229X) = okay_228X;
    arg0K0 = (1 + i_224X);
    goto L9205;}
  else {
    v_230X = s48_extantP(foo_227X);
    if (v_230X) {
      new_foo_231X = s48_trace_value(foo_227X);
      addr_232X = (((char *) (-3 + new_foo_231X))) + 12;
      S48_WRITE_BARRIER(new_foo_231X, addr_232X, okay_228X);
      *((long *) addr_232X) = okay_228X;
      arg0K0 = new_foo_231X;
      goto L9230;}
    else {
      arg0K0 = okay_228X;
      goto L9230;}}}
 L10314: {
  entry_233X = arg0K0;
  if ((1 == entry_233X)) {
    arg0K0 = (1 + i_226X);
    goto L10306;}
  else {
    addr_234X = (((char *) (-3 + entry_233X))) + 8;
    S48_WRITE_BARRIER(entry_233X, addr_234X, 529);
    *((long *) addr_234X) = 529;
    arg0K0 = (*((long *) ((((char *) (-3 + entry_233X))) + 12)));
    goto L10314;}}
 L9230: {
  v_235X = arg0K0;
  arg0K0 = (*((long *) ((((char *) (-3 + foo_227X))) + 12)));
  arg0K1 = v_235X;
  goto L9225;}
}
long s48_copy_symbol_table(void)
{
  long new_236X;
 {  new_236X = s48_trace_value((Sthe_symbol_tableS));
  HcleanerB2200(new_236X);
  return new_236X;}
}
void s48_setup_external_exception(long why_237X, long nargs_238X)
{

 {  push_exception_continuationB(why_237X, 1);
  if ((10 < nargs_238X)) {
    ps_error("too many arguments from external exception", 0);
    goto L11660;}
  else {
    goto L11660;}}
 L11660: {
  Sexternal_exception_nargsS = nargs_238X;
  Sexternal_exceptionPS = 1;
  return;}
}
void s48_close_channel(long os_index_239X)
{
  long obj_240X;
 {  if ((os_index_239X < 0)) {
    return;}
  else {
    if ((os_index_239X < (Snumber_of_channelsS))) {
      obj_240X = *((Svm_channelsS) + os_index_239X);
      if ((3 == (3 & obj_240X))) {
        if ((6 == (31 & ((((*((long *) ((((char *) (-3 + obj_240X))) + -4))))>>2))))) {
          close_channelB((*((Svm_channelsS) + os_index_239X)));
          return;}
        else {
          return;}}
      else {
        return;}}
    else {
      return;}}}
}
char s48_warn_about_undefined_imported_bindings(void)
{
  long arg0K0;
  long name_246X;
  long entry_245X;
  char temp_244X;
  long i_243X;
  long table_242X;
  FILE * out_241X;
 {  out_241X = stderr;
  table_242X = Simported_bindingsS;
  arg0K0 = 0;
  goto L12200;}
 L12200: {
  i_243X = arg0K0;
  temp_244X = 1024 == i_243X;
  if (temp_244X) {
    return temp_244X;}
  else {
    arg0K0 = (*((long *) ((((char *) (-3 + table_242X))) + (((i_243X)<<2)))));
    goto L12208;}}
 L12208: {
  entry_245X = arg0K0;
  if ((1 == entry_245X)) {
    arg0K0 = (1 + i_243X);
    goto L12200;}
  else {
    if ((17 == (255 & (*((long *) ((((char *) (-3 + entry_245X))) + 8)))))) {
      name_246X = *((long *) (((char *) (-3 + entry_245X))));
      ps_write_string("undefined imported name ", out_241X);
      if ((3 == (3 & name_246X))) {
        if ((17 == (31 & ((((*((long *) ((((char *) (-3 + name_246X))) + -4))))>>2))))) {
          ps_write_string((((char *)(((char *) (-3 + name_246X))))), out_241X);
          goto L12188;}
        else {
          goto L12186;}}
      else {
        goto L12186;}}
    else {
      goto L12211;}}}
 L12188: {
  { long ignoreXX;
  PS_WRITE_CHAR(10, out_241X, ignoreXX) }
  goto L12211;}
 L12186: {
  ps_write_string("<invalid name>", out_241X);
  goto L12188;}
 L12211: {
  arg0K0 = (*((long *) ((((char *) (-3 + entry_245X))) + 12)));
  goto L12208;}
}
void s48_define_exported_binding(char *name_247X, long value_248X)
{
  char arg2K0;
  long arg0K2;
  long arg0K1;
  long arg0K0;
  char * addr_261X;
  long x_260X;
  long name_259X;
  long value_258X;
  long key_257X;
  long temp_256X;
  long key_255X;
  char okayP_254X;
  char v_253X;
  long temp1_252X;
  long temp0_251X;
  char v_250X;
  long space_249X;
 {  space_249X = 6 + ((((4 + (strlen((char *) name_247X))))>>2));
  v_250X = AVAILABLEp(space_249X);
  if (v_250X) {
    arg2K0 = 1;
    arg0K1 = 0;
    arg0K2 = value_248X;
    goto L14980;}
  else {
    temp0_251X = collect_saving_temps(value_248X, 1, &temp1_252X);
    v_253X = AVAILABLEp(space_249X);
    if (v_253X) {
      arg2K0 = 1;
      arg0K1 = 0;
      arg0K2 = temp0_251X;
      goto L14980;}
    else {
      arg2K0 = 0;
      arg0K1 = 0;
      arg0K2 = temp0_251X;
      goto L14980;}}}
 L14980: {
  okayP_254X = arg2K0;
  key_255X = arg0K1;
  temp_256X = arg0K2;
  if (okayP_254X) {
    arg0K0 = key_255X;
    arg0K1 = temp_256X;
    goto L14961;}
  else {
    ps_error("Scheme48 heap overflow", 0);
    arg0K0 = key_255X;
    arg0K1 = temp_256X;
    goto L14961;}}
 L14961: {
  key_257X = arg0K0;
  value_258X = arg0K1;
  name_259X = enter_string(name_247X, key_257X);
  x_260X = Hlookup2123((Simported_bindingsS), name_259X, key_257X);
  addr_261X = (((char *) (-3 + x_260X))) + 8;
  S48_WRITE_BARRIER(x_260X, addr_261X, value_258X);
  *((long *) addr_261X) = value_258X;
  return;}
}
long s48_add_channel(long mode_262X, long id_263X, long os_index_264X)
{
  char arg2K0;
  long arg0K1;
  long arg0K0;
  long status_272X;
  long channel_271X;
  long v_270X;
  long key_269X;
  char okayP_268X;
  char v_267X;
  long temp1_266X;
  char v_265X;
 {  v_265X = AVAILABLEp(6);
  if (v_265X) {
    arg2K0 = 1;
    arg0K1 = 0;
    goto L15375;}
  else {
    collect_saving_temps(1, 1, &temp1_266X);
    v_267X = AVAILABLEp(6);
    if (v_267X) {
      arg2K0 = 1;
      arg0K1 = 0;
      goto L15375;}
    else {
      arg2K0 = 0;
      arg0K1 = 0;
      goto L15375;}}}
 L15375: {
  okayP_268X = arg2K0;
  key_269X = arg0K1;
  if (okayP_268X) {
    arg0K0 = key_269X;
    goto L15358;}
  else {
    ps_error("Scheme48 heap overflow", 0);
    arg0K0 = key_269X;
    goto L15358;}}
 L15358: {
  v_270X = arg0K0;
  channel_271X = make_registered_channel((((mode_262X)>>2)), id_263X, os_index_264X, v_270X, &status_272X);
  if ((3 == (3 & channel_271X))) {
    if ((6 == (31 & ((((*((long *) ((((char *) (-3 + channel_271X))) + -4))))>>2))))) {
      return channel_271X;}
    else {
      goto L15369;}}
  else {
    goto L15369;}}
 L15369: {
  return (((status_272X)<<2));}
}
long s48_get_imported_binding(char *name_273X)
{
  char arg2K0;
  long arg0K1;
  long arg0K0;
  long name_281X;
  long key_280X;
  long key_279X;
  char okayP_278X;
  char v_277X;
  long temp1_276X;
  char v_275X;
  long space_274X;
 {  space_274X = 6 + ((((4 + (strlen((char *) name_273X))))>>2));
  v_275X = AVAILABLEp(space_274X);
  if (v_275X) {
    arg2K0 = 1;
    arg0K1 = 0;
    goto L16064;}
  else {
    collect_saving_temps(1, 1, &temp1_276X);
    v_277X = AVAILABLEp(space_274X);
    if (v_277X) {
      arg2K0 = 1;
      arg0K1 = 0;
      goto L16064;}
    else {
      arg2K0 = 0;
      arg0K1 = 0;
      goto L16064;}}}
 L16064: {
  okayP_278X = arg2K0;
  key_279X = arg0K1;
  if (okayP_278X) {
    arg0K0 = key_279X;
    goto L16051;}
  else {
    ps_error("Scheme48 heap overflow", 0);
    arg0K0 = key_279X;
    goto L16051;}}
 L16051: {
  key_280X = arg0K0;
  name_281X = enter_string(name_273X, key_280X);
  return Hlookup2142((Sexported_bindingsS), name_281X, key_280X);}
}
long s48_allocate_stob(long type_282X, long size_283X)
{
  char * addr_287X;
  char v_286X;
  long temp1_285X;
  char v_284X;
 {  v_284X = AVAILABLEp(size_283X);
  if (v_284X) {
    goto L16359;}
  else {
    collect_saving_temps(1, 1, &temp1_285X);
    v_286X = AVAILABLEp(size_283X);
    if (v_286X) {
      goto L16359;}
    else {
      ps_error("Scheme48 heap overflow", 0);
      goto L16359;}}}
 L16359: {
  addr_287X = ALLOCATE_SPACE(type_282X, (4 + size_283X));
  *((long *) addr_287X) = (2 + (((((((size_283X)<<6)) + type_282X))<<2)));
  return (3 + (((long) (addr_287X + 4))));}
}
void s48_initialize_vm(char * stack_begin_288X, long stack_size_289X)
{
  char arg2K0;
  char * arg1K0;
  long arg0K2;
  long arg0K1;
  long arg0K0;
  char * addr_360X;
  long code_359X;
  long temp_358X;
  char * addr_357X;
  char * addr_356X;
  char * addr_355X;
  long code_354X;
  long temp_353X;
  char * addr_352X;
  char * addr_351X;
  char v_350X;
  long temp1_349X;
  char v_348X;
  long cont_347X;
  char * addr_346X;
  long code_345X;
  long temp_344X;
  char * addr_343X;
  char * addr_342X;
  char v_341X;
  long temp1_340X;
  char v_339X;
  char * a_338X;
  long size_337X;
  char * start_336X;
  char * stack_335X;
  long v_334X;
  char * addr_333X;
  long i_332X;
  char * addr_331X;
  char * addr_330X;
  long val_329X;
  long index_328X;
  long h_327X;
  long i_326X;
  long table_325X;
  char * addr_324X;
  long v_323X;
  char * addr_322X;
  long i_321X;
  long n_320X;
  long string_319X;
  long foo_318X;
  long table_317X;
  long i_316X;
  long table_315X;
  char * addr_314X;
  long exported_bindings_313X;
  long imported_bindings_312X;
  long n_311X;
  long symbols_310X;
  long temp1_309X;
  long temp0_308X;
  char okayP_307X;
  char * addr_306X;
  long maybe_305X;
  long v_304X;
  long maybe_303X;
  long i_302X;
  char v_301X;
  long temp1_300X;
  long temp0_299X;
  char v_298X;
  long imported_bindings_297X;
  long exported_bindings_296X;
  long table_295X;
  char * addr_294X;
  char v_293X;
  long temp1_292X;
  char v_291X;
  long symbol_table_290X;
 {  symbol_table_290X = s48_initial_symbols();
  if ((symbol_table_290X == 1)) {
    v_291X = AVAILABLEp(1025);
    if (v_291X) {
      goto L16090;}
    else {
      collect_saving_temps(1, 1, &temp1_292X);
      v_293X = AVAILABLEp(1025);
      if (v_293X) {
        goto L16090;}
      else {
        ps_error("Scheme48 heap overflow", 0);
        goto L16090;}}}
  else {
    Sthe_symbol_tableS = symbol_table_290X;
    goto L16482;}}
 L16090: {
  addr_294X = ALLOCATE_SPACE(2, 4100);
  *((long *) addr_294X) = 1048586;
  table_295X = 3 + (((long) (addr_294X + 4)));
  arg0K0 = 0;
  goto L16165;}
 L16482: {
  exported_bindings_296X = s48_initial_exported_bindings();
  imported_bindings_297X = s48_initial_imported_bindings();
  v_298X = AVAILABLEp(2050);
  if (v_298X) {
    arg2K0 = 1;
    arg0K1 = imported_bindings_297X;
    arg0K2 = exported_bindings_296X;
    goto L15062;}
  else {
    temp0_299X = collect_saving_temps(imported_bindings_297X, exported_bindings_296X, &temp1_300X);
    v_301X = AVAILABLEp(2050);
    if (v_301X) {
      arg2K0 = 1;
      arg0K1 = temp0_299X;
      arg0K2 = temp1_300X;
      goto L15062;}
    else {
      arg2K0 = 0;
      arg0K1 = temp0_299X;
      arg0K2 = temp1_300X;
      goto L15062;}}}
 L16165: {
  i_302X = arg0K0;
  if ((1024 == i_302X)) {
    Sthe_symbol_tableS = table_295X;
    maybe_303X = s48_find_all(1);
    if ((maybe_303X == 1)) {
      collect_saving_temps(0, 0, &v_304X);
      maybe_305X = s48_find_all(1);
      if ((maybe_305X == 1)) {
        ps_error("insufficient heap space to build symbol table", 0);
        arg0K0 = maybe_305X;
        goto L16119;}
      else {
        arg0K0 = maybe_305X;
        goto L16119;}}
    else {
      arg0K0 = maybe_303X;
      goto L16119;}}
  else {
    addr_306X = (((char *) (-3 + table_295X))) + (((i_302X)<<2));
    S48_WRITE_BARRIER(table_295X, addr_306X, 1);
    *((long *) addr_306X) = 1;
    arg0K0 = (1 + i_302X);
    goto L16165;}}
 L15062: {
  okayP_307X = arg2K0;
  temp0_308X = arg0K1;
  temp1_309X = arg0K2;
  if (okayP_307X) {
    arg0K0 = temp0_308X;
    arg0K1 = temp1_309X;
    goto L15023;}
  else {
    ps_error("Scheme48 heap overflow", 0);
    arg0K0 = temp0_308X;
    arg0K1 = temp1_309X;
    goto L15023;}}
 L16119: {
  symbols_310X = arg0K0;
  n_311X = (((3 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + symbols_310X))) + -4))))>>8))))>>2);
  arg0K0 = 0;
  goto L16186;}
 L15023: {
  imported_bindings_312X = arg0K0;
  exported_bindings_313X = arg0K1;
  if ((1 == imported_bindings_312X)) {
    addr_314X = ALLOCATE_SPACE(2, 4100);
    *((long *) addr_314X) = 1048586;
    table_315X = 3 + (((long) (addr_314X + 4)));
    arg0K0 = 0;
    goto L15124;}
  else {
    arg0K0 = imported_bindings_312X;
    goto L15038;}}
 L16186: {
  i_316X = arg0K0;
  if ((i_316X == n_311X)) {
    goto L16482;}
  else {
    table_317X = Sthe_symbol_tableS;
    foo_318X = *((long *) ((((char *) (-3 + symbols_310X))) + (((i_316X)<<2))));
    string_319X = *((long *) (((char *) (-3 + foo_318X))));
    n_320X = -1 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + string_319X))) + -4))))>>8));
    arg0K0 = 0;
    arg0K1 = 0;
    goto L13647;}}
 L15124: {
  i_321X = arg0K0;
  if ((1024 == i_321X)) {
    arg0K0 = table_315X;
    goto L15038;}
  else {
    addr_322X = (((char *) (-3 + table_315X))) + (((i_321X)<<2));
    S48_WRITE_BARRIER(table_315X, addr_322X, 1);
    *((long *) addr_322X) = 1;
    arg0K0 = (1 + i_321X);
    goto L15124;}}
 L15038: {
  v_323X = arg0K0;
  Simported_bindingsS = v_323X;
  if ((1 == exported_bindings_313X)) {
    addr_324X = ALLOCATE_SPACE(2, 4100);
    *((long *) addr_324X) = 1048586;
    table_325X = 3 + (((long) (addr_324X + 4)));
    arg0K0 = 0;
    goto L15101;}
  else {
    arg0K0 = exported_bindings_313X;
    goto L15051;}}
 L13647: {
  i_326X = arg0K0;
  h_327X = arg0K1;
  if ((i_326X < n_320X)) {
    arg0K0 = (1 + i_326X);
    arg0K1 = (h_327X + (((*((unsigned char *) ((((char *) (-3 + string_319X))) + i_326X))))));
    goto L13647;}
  else {
    index_328X = 1023 & h_327X;
    val_329X = *((long *) ((((char *) (-3 + table_317X))) + (((index_328X)<<2))));
    addr_330X = (((char *) (-3 + foo_318X))) + 4;
    S48_WRITE_BARRIER(foo_318X, addr_330X, val_329X);
    *((long *) addr_330X) = val_329X;
    addr_331X = (((char *) (-3 + table_317X))) + (((index_328X)<<2));
    S48_WRITE_BARRIER(table_317X, addr_331X, foo_318X);
    *((long *) addr_331X) = foo_318X;
    arg0K0 = (1 + i_316X);
    goto L16186;}}
 L15101: {
  i_332X = arg0K0;
  if ((1024 == i_332X)) {
    arg0K0 = table_325X;
    goto L15051;}
  else {
    addr_333X = (((char *) (-3 + table_325X))) + (((i_332X)<<2));
    S48_WRITE_BARRIER(table_325X, addr_333X, 1);
    *((long *) addr_333X) = 1;
    arg0K0 = (1 + i_332X);
    goto L15101;}}
 L15051: {
  v_334X = arg0K0;
  Sexported_bindingsS = v_334X;
  if ((stack_size_289X < 8131)) {
    stack_335X = (char *)malloc(32524);
    if ((stack_335X == NULL)) {
      ps_error("out of memory, unable to continue", 0);
      arg1K0 = stack_335X;
      arg0K1 = 8131;
      goto L15576;}
    else {
      arg1K0 = stack_335X;
      arg0K1 = 8131;
      goto L15576;}}
  else {
    arg1K0 = stack_begin_288X;
    arg0K1 = stack_size_289X;
    goto L15576;}}
 L15576: {
  start_336X = arg1K0;
  size_337X = arg0K1;
  Sstack_beginS = start_336X;
  Sstack_endS = (start_336X + (((size_337X)<<2)));
  Sstack_limitS = ((Sstack_beginS) + 524);
  SstackS = ((Sstack_endS) + -4);
  ScontS = 1;
  SenvS = 17;
  arg1K0 = start_336X;
  goto L15608;}
 L15608: {
  a_338X = arg1K0;
  if ((a_338X == (Sstack_endS))) {
    v_339X = AVAILABLEp(5);
    if (v_339X) {
      goto L15625;}
    else {
      collect_saving_temps(1, 1, &temp1_340X);
      v_341X = AVAILABLEp(5);
      if (v_341X) {
        goto L15625;}
      else {
        ps_error("Scheme48 heap overflow", 0);
        goto L15625;}}}
  else {
    *((long *) a_338X) = 252645135;
    arg1K0 = (a_338X + 4);
    goto L15608;}}
 L15625: {
  addr_342X = ALLOCATE_SPACE(18, 6);
  *((long *) addr_342X) = 586;
  addr_343X = ALLOCATE_SPACE(12, 12);
  *((long *) addr_343X) = 2098;
  temp_344X = 3 + (((long) (addr_343X + 4)));
  code_345X = 3 + (((long) (addr_342X + 4)));
  addr_346X = ((char *) (-3 + temp_344X));
  S48_WRITE_BARRIER(temp_344X, addr_346X, code_345X);
  *((long *) addr_346X) = code_345X;
  *((unsigned char *) (((char *) (-3 + code_345X)))) = 23;
  *((unsigned char *) ((((char *) (-3 + code_345X))) + 1)) = 31;
  SstackS = ((SstackS) + -20);
  *((long *) ((SstackS) + 4)) = 4266;
  cont_347X = 3 + (((long) ((SstackS) + 8)));
  *((long *) ((((char *) (-3 + cont_347X))) + 4)) = 0;
  *((long *) ((((char *) (-3 + cont_347X))) + 8)) = temp_344X;
  *((long *) ((((char *) (-3 + cont_347X))) + 12)) = (SenvS);
  *((long *) (((char *) (-3 + cont_347X)))) = (ScontS);
  ScontS = cont_347X;
  Sbottom_of_stackS = (ScontS);
  v_348X = AVAILABLEp(5);
  if (v_348X) {
    goto L15804;}
  else {
    collect_saving_temps(1, 1, &temp1_349X);
    v_350X = AVAILABLEp(5);
    if (v_350X) {
      goto L15804;}
    else {
      ps_error("Scheme48 heap overflow", 0);
      goto L15804;}}}
 L15804: {
  addr_351X = ALLOCATE_SPACE(18, 6);
  *((long *) addr_351X) = 586;
  addr_352X = ALLOCATE_SPACE(12, 12);
  *((long *) addr_352X) = 2098;
  temp_353X = 3 + (((long) (addr_352X + 4)));
  code_354X = 3 + (((long) (addr_351X + 4)));
  addr_355X = ((char *) (-3 + temp_353X));
  S48_WRITE_BARRIER(temp_353X, addr_355X, code_354X);
  *((long *) addr_355X) = code_354X;
  *((unsigned char *) (((char *) (-3 + code_354X)))) = 34;
  *((unsigned char *) ((((char *) (-3 + code_354X))) + 1)) = 142;
  Sinterrupt_templateS = temp_353X;
  addr_356X = ALLOCATE_SPACE(18, 6);
  *((long *) addr_356X) = 586;
  addr_357X = ALLOCATE_SPACE(12, 12);
  *((long *) addr_357X) = 2098;
  temp_358X = 3 + (((long) (addr_357X + 4)));
  code_359X = 3 + (((long) (addr_356X + 4)));
  addr_360X = ((char *) (-3 + temp_358X));
  S48_WRITE_BARRIER(temp_358X, addr_360X, code_359X);
  *((long *) addr_360X) = code_359X;
  *((unsigned char *) (((char *) (-3 + code_359X)))) = 139;
  *((unsigned char *) ((((char *) (-3 + code_359X))) + 1)) = 125;
  Sexception_templateS = temp_358X;
  return;}
}
long s48_restart(long proc_361X, long nargs_362X)
{
  char * arg1K1;
  char * arg1K0;
  char *arg3K0;
  char arg2K3;
  char arg2K1;
  char arg2K2;
  char arg2K0;
  long arg0K3;
  long arg0K2;
  long arg0K1;
  long arg0K0;
  char *merged_arg3K0;
  long merged_arg0K3;
  long merged_arg0K2;
  long merged_arg0K1;
  long merged_arg0K0;

  int pop_continuationB_return_tag;
  int get_error_string_return_tag;
  long get_error_string0_return_value;
  int okay_argument_list_return_tag;
  char okay_argument_list0_return_value;
  long okay_argument_list1_return_value;
  int get_current_port_return_tag;
  long get_current_port0_return_value;
  int copy_continuation_from_heapB_return_tag;
  long copy_continuation_from_heapB0_return_value;
  int collect_saving_temp_return_tag;
  long collect_saving_temp0_return_value;
  int copy_listS_return_tag;
  long copy_listS0_return_value;
  int pop_args_GlistS_return_tag;
  long pop_args_GlistS0_return_value;
  int save_env_in_heap_return_tag;
  long save_env_in_heap0_return_value;
  int copy_env_return_tag;
  long copy_env0_return_value;
  int really_preserve_continuation_return_tag;
  long really_preserve_continuation0_return_value;
  int copy_stack_into_heap_return_tag;
  int push_list_return_tag;
  long push_list0_return_value;
  int rest_list_setup_return_tag;
  int check_events_return_tag;
  char check_events0_return_value;
  int loseD0_return_tag;
  long status_363X;
  long list_364X;
  long marker_365X;
  long cont_366X;
  long value_367X;
  long list_368X;
  long length_369X;
  long start_370X;
  long count_371X;
  long env_372X;
  long cont_373X;
  long key_374X;
  long reason_375X;
  long env_376X;
  long key_377X;
  long reason_378X;
  long key_379X;
  long list_380X;
  long count_381X;
  long wants_stack_args_382X;
  long stack_arg_count_383X;
  long list_args_384X;
  long list_arg_count_385X;
  char *message_386X;
  long pc_1334X;
  long tem_1333X;
  long cont_1332X;
  long i_1331X;
  long string_1330X;
  char * addr_1329X;
  long len_1328X;
  long len_1327X;
  long x_1326X;
  char *string_1325X;
  char move_slowP_1324X;
  long slow_1323X;
  long len_1322X;
  long fast_1321X;
  long v_1320X;
  char *v_1319X;
  long v_1318X;
  long v_1317X;
  long obj_1316X;
  long env_1315X;
  long thread_1314X;
  long v_1313X;
  long new_cont_1312X;
  char * top_1311X;
  long v_1310X;
  long value_1309X;
  char * addr_1308X;
  long x_1307X;
  char * addr_1306X;
  long a_1305X;
  long last_1304X;
  long l_1303X;
  long x_1302X;
  char * addr_1301X;
  long a_1300X;
  long list_1299X;
  long temp_1298X;
  char okayP_1297X;
  char v_1296X;
  long temp1_1295X;
  long temp0_1294X;
  char v_1293X;
  long space_1292X;
  long x_1291X;
  char * addr_1290X;
  long a_1289X;
  long count_1288X;
  long args_1287X;
  long start_1286X;
  long temp_1285X;
  char okayP_1284X;
  char v_1283X;
  long temp1_1282X;
  long temp0_1281X;
  char v_1280X;
  long space_1279X;
  long env_1278X;
  long cont_1277X;
  char * addr_1276X;
  long new_1275X;
  long p_1274X;
  long env_1273X;
  long top_1272X;
  char * addr_1271X;
  long new_1270X;
  char * data_addr_1269X;
  char * addr_1268X;
  long header_1267X;
  long new_1266X;
  char * data_addr_1265X;
  char * addr_1264X;
  long header_1263X;
  long p_1262X;
  long previous_1261X;
  long cont_1260X;
  long end_1259X;
  long v_1258X;
  long p_1257X;
  char * arg_1256X;
  char * loc_1255X;
  long stob_1254X;
  char * top_1253X;
  long arg_count_1252X;
  long i_1251X;
  char * p_1250X;
  long v_1249X;
  long l_1248X;
  long i_1247X;
  long key_1246X;
  char okayP_1245X;
  long list_1244X;
  char v_1243X;
  long temp1_1242X;
  char v_1241X;
  long space_1240X;
  long x_1239X;
  long v_1238X;
  long x_1237X;
  long v_1236X;
  long count_1235X;
  long x_1234X;
  long interrupt_bit_1233X;
  long status_1232X;
  long channel_1231X;
  long type_1230X;
  char v_1229X;
  long template_1228X;
  char not_firstP_1227X;
  long cont_1226X;
  char not_firstP_1225X;
  FILE * out_1224X;
  long current_template_1223X;
  long why_1222X;
  long x_1221X;
  long x_1220X;
  long x_1219X;
  char * addr_1218X;
  long status_1217X;
  char pendingP_1216X;
  char eofP_1215X;
  long got_1214X;
  long length_1213X;
  long v_1212X;
  long obj_1211X;
  long reason_1210X;
  long status_1209X;
  long v_1208X;
  long v_1207X;
  long key_1206X;
  char okayP_1205X;
  char v_1204X;
  char * addr_1203X;
  long value_1202X;
  long offset_1201X;
  long i_1200X;
  long count_1199X;
  long y_1198X;
  char * addr_1197X;
  long val_1196X;
  char * addr_1195X;
  long value_1194X;
  long len_1193X;
  long s2_1192X;
  long foo_1191X;
  long previous_foo_1190X;
  long val_1189X;
  char * addr_1188X;
  char * addr_1187X;
  long val_1186X;
  long x_1185X;
  char * addr_1184X;
  long status_1183X;
  char pendingP_1182X;
  long got_1181X;
  long length_1180X;
  long count_1179X;
  long start_1178X;
  char waitP_1177X;
  long x_1176X;
  long status_1175X;
  long channel_1174X;
  long v_1173X;
  long v_1172X;
  long v_1171X;
  long reason_1170X;
  long channel_1169X;
  long index_1168X;
  long val_1167X;
  long final_stack_arg_count_1166X;
  char v_1165X;
  long temp1_1164X;
  char v_1163X;
  long space_1162X;
  long stack_arg_count_1161X;
  long stack_slots_1160X;
  long x_1159X;
  char x_1158X;
  long channel_1157X;
  long x_1156X;
  char x_1155X;
  long channel_1154X;
  long obj_1153X;
  long arg_count_1152X;
  long count_1151X;
  long i_1150X;
  long env_1149X;
  long value_1148X;
  char *v_1147X;
  long y_1146X;
  long i_1145X;
  long l_1144X;
  long bucket_1143X;
  long index_1142X;
  long h_1141X;
  long i_1140X;
  long v_1139X;
  long v_1138X;
  long x_1137X;
  long status_1136X;
  long reason_1135X;
  long status_1134X;
  long undumpable_count_1133X;
  long undumpables_1132X;
  long status_1131X;
  long status_1130X;
  long resume_proc_1129X;
  long v_1128X;
  long n_1127X;
  char * addr_1126X;
  long prev_1125X;
  long ch_1124X;
  long val_1123X;
  long val_1122X;
  long val_1121X;
  long reason_1120X;
  long x_1119X;
  long obj_1118X;
  long v_1117X;
  long v_1116X;
  long len_1115X;
  long s2_1114X;
  char * addr_1113X;
  long x_1112X;
  char * addr_1111X;
  long foo_1110X;
  long rest_list_1109X;
  long i_1108X;
  long n_1107X;
  long args_1106X;
  long skip_1105X;
  long template_1104X;
  long stack_arg_count_1103X;
  long skip_1102X;
  long skip_1101X;
  long template_1100X;
  long stack_arg_count_1099X;
  long skip_1098X;
  long x_1097X;
  char * addr_1096X;
  long next_1095X;
  long channel_1094X;
  char * addr_1093X;
  long next_1092X;
  long channel_1091X;
  long m_1090X;
  long i_1089X;
  long value_1088X;
  long back_1087X;
  long env_1086X;
  long offset_1085X;
  long i_1084X;
  long x_1083X;
  char * addr_1082X;
  long a_1081X;
  long env_1080X;
  long v_1079X;
  long p_1078X;
  long key_1077X;
  long string_1076X;
  char * addr_1075X;
  long len_1074X;
  long n_1073X;
  long table_1072X;
  long x_1071X;
  long arg2_1070X;
  long key_1069X;
  char temp_1068X;
  char minutesP_1067X;
  long status_1066X;
  long v_1065X;
  char v_1064X;
  long status_1063X;
  long status_1062X;
  FILE * port_1061X;
  char x_1060X;
  long comment_string_1059X;
  long obj_1058X;
  long arg3_1057X;
  long arg2_1056X;
  long key_1055X;
  long x_1054X;
  char * addr_1053X;
  long channel_1052X;
  long res_1051X;
  long i_1050X;
  char * addr_1049X;
  long next_1048X;
  long channel_1047X;
  char * addr_1046X;
  long next_1045X;
  long channel_1044X;
  long n_1043X;
  char * addr_1042X;
  long head_1041X;
  long x_1040X;
  long status_1039X;
  char readyP_1038X;
  long channel_1037X;
  long obj_1036X;
  long key_1035X;
  char x_1034X;
  char inputP_1033X;
  char v_1032X;
  long channel_1031X;
  long count_1030X;
  long start_1029X;
  long obj_1028X;
  long arg4_1027X;
  long arg3_1026X;
  long arg2_1025X;
  long key_1024X;
  long arg5_1023X;
  long arg4_1022X;
  long arg3_1021X;
  long arg2_1020X;
  long key_1019X;
  long x_1018X;
  long status_1017X;
  long channel_1016X;
  long obj_1015X;
  long key_1014X;
  long mode_1013X;
  long arg2_1012X;
  long key_1011X;
  long bucket_1010X;
  long index_1009X;
  long h_1008X;
  long i_1007X;
  long i_1006X;
  long i_1005X;
  char * addr_1004X;
  long i_1003X;
  long i_1002X;
  long i_1001X;
  long c_1000X;
  long b_999X;
  long c_998X;
  long b_997X;
  char x_996X;
  long c_995X;
  long b_994X;
  long c_993X;
  long mid_c_992X;
  long v_991X;
  long v_990X;
  long lo_c_989X;
  long hi_b_988X;
  long hi_a_987X;
  long lo_b_986X;
  long lo_a_985X;
  long b_984X;
  long v_983X;
  long v_982X;
  long args_981X;
  long next_980X;
  long next_op_979X;
  long cont_978X;
  long index_977X;
  long length_976X;
  long wants_stack_args_975X;
  long skip_974X;
  long skip_973X;
  long stack_space_972X;
  long protocol_971X;
  long index_970X;
  long length_969X;
  long wants_stack_args_968X;
  long skip_967X;
  long skip_966X;
  long stack_space_965X;
  long protocol_964X;
  long n_963X;
  long tem_962X;
  long stack_arg_count_961X;
  long v_960X;
  long v_959X;
  long key_958X;
  long start_i_957X;
  long temp_956X;
  char okayP_955X;
  long key_954X;
  char okayP_953X;
  long obj_952X;
  long obj_951X;
  long type_950X;
  long thing_949X;
  long stuff_948X;
  char * addr_947X;
  long val_946X;
  long x_945X;
  long i_944X;
  long b_943X;
  long p_942X;
  long port_941X;
  long Kchar_940X;
  long x_939X;
  long i_938X;
  long b_937X;
  long p_936X;
  long p_935X;
  long port_934X;
  char * addr_933X;
  long val_932X;
  long x_931X;
  long i_930X;
  long b_929X;
  long p_928X;
  long p_927X;
  long port_926X;
  long list_925X;
  long head_924X;
  char move_slowP_923X;
  long slow_922X;
  long list_921X;
  long n_920X;
  long arg2_919X;
  long val_918X;
  long mseconds_917X;
  long seconds_916X;
  long option_915X;
  long key_914X;
  char okayP_913X;
  long result_912X;
  char * args_911X;
  long name_910X;
  long proc_909X;
  long rest_list_908X;
  long x_907X;
  long tem_906X;
  long pc_905X;
  long v_904X;
  long vector_903X;
  long type_902X;
  char firstP_901X;
  long v_900X;
  long vector_899X;
  char firstP_898X;
  long x_897X;
  char * addr_896X;
  long b_895X;
  long x_894X;
  char * addr_893X;
  long proc_892X;
  long stob_891X;
  long h_890X;
  long i_889X;
  long key_888X;
  char okayP_887X;
  char inputP_886X;
  long key_885X;
  char okayP_884X;
  long key_883X;
  char okayP_882X;
  long key_881X;
  char okayP_880X;
  long key_879X;
  char okayP_878X;
  long key_877X;
  char okayP_876X;
  long val_875X;
  char * addr_874X;
  char * addr_873X;
  long val_872X;
  long n_871X;
  long string_870X;
  long table_869X;
  long obj_868X;
  long string_867X;
  char * addr_866X;
  long len_865X;
  long vector_864X;
  char * addr_863X;
  long value_862X;
  char * addr_861X;
  long len_860X;
  long init_859X;
  char okayP_858X;
  long rest_list_857X;
  long stack_nargs_856X;
  long new_855X;
  char * addr_854X;
  long len_853X;
  long type_852X;
  long new_851X;
  char * addr_850X;
  long len_849X;
  long type_848X;
  long value_847X;
  long val_846X;
  long val_845X;
  long val_844X;
  long val_843X;
  long r_842X;
  long n_841X;
  long a_840X;
  long a_839X;
  long val_838X;
  long val_837X;
  long val_836X;
  long val_835X;
  long val_834X;
  long a_833X;
  long a_832X;
  long val_831X;
  long val_830X;
  long delta_829X;
  long delta_828X;
  long offset_827X;
  long key_826X;
  char okayP_825X;
  char v_824X;
  long tem_823X;
  long key_822X;
  char okayP_821X;
  long list_arg_count_820X;
  long list_args_819X;
  long stack_nargs_818X;
  long obj_817X;
  char * addr_816X;
  long list_args_815X;
  long follower_814X;
  long list_813X;
  long args_812X;
  long list_arg_count_811X;
  char okayP_810X;
  long stack_nargs_809X;
  long list_args_808X;
  long total_arg_count_807X;
  long code_806X;
  long obj_805X;
  long obj_804X;
  long list_arg_count_803X;
  long list_args_802X;
  long stack_arg_count_801X;
  long obj_800X;
  char * arg_799X;
  char * loc_798X;
  long args_797X;
  long v_796X;
  long v_795X;
  long v_794X;
  long list_arg_count_793X;
  long list_args_792X;
  long stack_arg_count_791X;
  long exception_790X;
  long code_789X;
  long stack_arg_count_788X;
  char v_787X;
  long key_786X;
  char okayP_785X;
  long key_784X;
  char okayP_783X;
  char * addr_782X;
  long value_781X;
  long v_780X;
  long new_env_779X;
  char * addr_778X;
  long len_777X;
  char v_776X;
  long temp1_775X;
  long temp0_774X;
  char v_773X;
  long env_772X;
  char v_771X;
  long temp1_770X;
  char v_769X;
  long space_768X;
  char * addr_767X;
  long value_766X;
  long index_765X;
  long i_764X;
  long env_763X;
  long i_762X;
  long env_761X;
  long i_760X;
  long env_759X;
  long obj_758X;
  long opcode_757X;
  long nargs_756X;
  FILE * out_755X;
  long x_754X;
  long v_753X;
  long v_752X;
  long v_751X;
  long v_750X;
  long v_749X;
  long v_748X;
  long v_747X;
  long count_746X;
  long to_index_745X;
  long from_index_744X;
  long arg5_743X;
  long arg4_742X;
  long arg3_741X;
  long arg2_740X;
  char * addr_739X;
  long len_738X;
  long value_737X;
  long index_736X;
  long arg4_735X;
  long arg3_734X;
  long arg2_733X;
  long len_732X;
  long index_731X;
  long arg3_730X;
  long arg2_729X;
  long list_728X;
  long thing_727X;
  char v_726X;
  long temp1_725X;
  char v_724X;
  long space_723X;
  long len_722X;
  long x_721X;
  long obj_720X;
  long arg2_719X;
  long x_718X;
  long arg2_717X;
  long status_716X;
  long value_715X;
  long key_714X;
  long arg2_713X;
  long mseconds_712X;
  long seconds_711X;
  long mseconds_710X;
  long seconds_709X;
  long x_708X;
  long other_707X;
  long option_706X;
  long arg2_705X;
  long x_704X;
  long arg2_703X;
  char v_702X;
  long temp1_701X;
  char v_700X;
  long rest_list_699X;
  long p_698X;
  long nargs_697X;
  long x_696X;
  long arg2_695X;
  long p_694X;
  long p_693X;
  long p_692X;
  long old_691X;
  long temp_690X;
  long obj_689X;
  long p_688X;
  long template_687X;
  long p_686X;
  long temp_685X;
  long obj_684X;
  long x_683X;
  long type_682X;
  long x_681X;
  long bytes_680X;
  long x_679X;
  long other_678X;
  long key_677X;
  long arg2_676X;
  char v_675X;
  long temp1_674X;
  char v_673X;
  long n_672X;
  long x_671X;
  long obj_670X;
  long v_669X;
  char v_668X;
  long temp1_667X;
  char v_666X;
  long x_665X;
  char v_664X;
  long temp1_663X;
  char v_662X;
  long space_661X;
  char temp_660X;
  long channel_659X;
  long obj_658X;
  char v_657X;
  long temp1_656X;
  char v_655X;
  long x_654X;
  long arg2_653X;
  char v_652X;
  long temp1_651X;
  char v_650X;
  char v_649X;
  long temp1_648X;
  char v_647X;
  char v_646X;
  long temp1_645X;
  char v_644X;
  char v_643X;
  long temp1_642X;
  char v_641X;
  long x_640X;
  long x_639X;
  long x_638X;
  long arg2_637X;
  long descriptor_636X;
  long x_635X;
  long obj_634X;
  char v_633X;
  long temp1_632X;
  char v_631X;
  long len_630X;
  char Kchar_629X;
  long index_628X;
  long arg3_627X;
  long arg2_626X;
  long len_625X;
  long index_624X;
  long arg2_623X;
  long obj_622X;
  char v_621X;
  long temp1_620X;
  char v_619X;
  long size_618X;
  char init_617X;
  long len_616X;
  long arg2_615X;
  long len_614X;
  long Kchar_613X;
  long index_612X;
  long arg3_611X;
  long arg2_610X;
  long len_609X;
  long index_608X;
  long arg2_607X;
  long obj_606X;
  char v_605X;
  long temp1_604X;
  char v_603X;
  long size_602X;
  long init_601X;
  long len_600X;
  long arg2_599X;
  char * addr_598X;
  long index_597X;
  long len_596X;
  long type_595X;
  long value_594X;
  long stob_593X;
  long arg2_592X;
  long index_591X;
  long len_590X;
  long type_589X;
  long index_588X;
  long stob_587X;
  char v_586X;
  long temp1_585X;
  long temp0_584X;
  char v_583X;
  long size_582X;
  long len_581X;
  long type_580X;
  long init_579X;
  long len_578X;
  char * addr_577X;
  long type_576X;
  long offset_575X;
  long value_574X;
  long stob_573X;
  long type_572X;
  long offset_571X;
  long stob_570X;
  char v_569X;
  long temp1_568X;
  char v_567X;
  long space_566X;
  long len_565X;
  char v_564X;
  long temp1_563X;
  char v_562X;
  long space_561X;
  long len_560X;
  long type_559X;
  long stob_558X;
  long type_557X;
  long x_556X;
  long x_555X;
  long x_554X;
  long x_553X;
  long arg2_552X;
  long x_551X;
  long arg2_550X;
  long x_549X;
  long x_548X;
  long result_547X;
  long x_546X;
  long x_545X;
  long count_544X;
  long value_543X;
  long y_542X;
  long x_541X;
  long x_540X;
  long arg2_539X;
  long x_538X;
  long arg2_537X;
  long x_536X;
  long arg2_535X;
  long x_534X;
  long x_533X;
  long arg2_532X;
  long x_531X;
  long arg2_530X;
  long x_529X;
  long arg2_528X;
  long n_527X;
  long n_526X;
  long x_525X;
  long x_524X;
  long x_523X;
  long x_522X;
  long x_521X;
  long x_520X;
  long x_519X;
  long x_518X;
  long x_517X;
  long x_516X;
  long x_515X;
  long x_514X;
  long x_513X;
  long x_512X;
  long a_511X;
  long y_510X;
  long x_509X;
  long b_508X;
  long a_507X;
  long x_506X;
  long arg2_505X;
  long x_504X;
  long arg2_503X;
  long x_502X;
  long arg2_501X;
  long x_500X;
  long arg2_499X;
  long x_498X;
  long arg2_497X;
  long x_496X;
  long arg2_495X;
  long b_494X;
  long a_493X;
  long x_492X;
  long arg2_491X;
  long z_490X;
  long y_489X;
  long x_488X;
  long b_487X;
  long a_486X;
  long x_485X;
  long arg2_484X;
  long z_483X;
  long y_482X;
  long x_481X;
  long x_480X;
  long x_479X;
  long x_478X;
  long n_477X;
  long n_476X;
  long n_475X;
  long n_474X;
  long x_473X;
  long x_472X;
  long arg2_471X;
  long rest_list_470X;
  long x_469X;
  long rest_list_468X;
  long stack_nargs_467X;
  long arg0_466X;
  long arg1_465X;
  long rest_list_464X;
  long arg0_463X;
  long stack_nargs_462X;
  long index_461X;
  long val_460X;
  long max_459X;
  long p_458X;
  char v_457X;
  long temp1_456X;
  char v_455X;
  long space_454X;
  long space_453X;
  long index_452X;
  long v_451X;
  char v_450X;
  long temp1_449X;
  char v_448X;
  long space_447X;
  long code_446X;
  long tem_445X;
  long stack_nargs_444X;
  long p_443X;
  long obj_442X;
  long consumer_441X;
  long cont_440X;
  long rest_list_439X;
  long proc_438X;
  long stack_nargs_437X;
  long args_436X;
  long length_435X;
  char okayP_434X;
  long list_args_433X;
  long stob_432X;
  char * top_of_args_431X;
  long nargs_430X;
  long obj_429X;
  long stack_arg_count_428X;
  char v_427X;
  long temp1_426X;
  char v_425X;
  long space_424X;
  long code_423X;
  long template_422X;
  long obj_421X;
  long stack_arg_count_420X;
  long cont_419X;
  char v_418X;
  long temp1_417X;
  char v_416X;
  long space_415X;
  char v_414X;
  long temp1_413X;
  char v_412X;
  long space_411X;
  long total_count_410X;
  long p_409X;
  long v_408X;
  char * addr_407X;
  long val_406X;
  long location_405X;
  long location_404X;
  long env_403X;
  long back_402X;
  long env_401X;
  long back_400X;
  long env_399X;
  long back_398X;
  long count_397X;
  char * code_pointer_396X;
  long code_395X;
  char * addr_394X;
  long proc_393X;
  long temp_392X;
  char okayP_391X;
  char v_390X;
  long temp1_389X;
  long temp0_388X;
  char v_387X;
 {  v_387X = AVAILABLEp(2);
  if (v_387X) {
    arg2K0 = 1;
    arg0K1 = proc_361X;
    goto L20030;}
  else {
    temp0_388X = collect_saving_temps(proc_361X, 1, &temp1_389X);
    v_390X = AVAILABLEp(2);
    if (v_390X) {
      arg2K0 = 1;
      arg0K1 = temp0_388X;
      goto L20030;}
    else {
      arg2K0 = 0;
      arg0K1 = temp0_388X;
      goto L20030;}}}
 L20030: {
  okayP_391X = arg2K0;
  temp_392X = arg0K1;
  if (okayP_391X) {
    arg0K0 = temp_392X;
    goto L20001;}
  else {
    ps_error("Scheme48 heap overflow", 0);
    arg0K0 = temp_392X;
    goto L20001;}}
 L20001: {
  proc_393X = arg0K0;
  addr_394X = ALLOCATE_SPACE(18, 6);
  *((long *) addr_394X) = 586;
  code_395X = 3 + (((long) (addr_394X + 4)));
  *((unsigned char *) (((char *) (-3 + code_395X)))) = 24;
  *((unsigned char *) ((((char *) (-3 + code_395X))) + 1)) = nargs_362X;
  Scode_pointerS = (((char *) (-3 + code_395X)));
  SvalS = proc_393X;
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L19093: {
  code_pointer_396X = arg1K0;
  switch ((*((unsigned char *) code_pointer_396X))) {
    case 0 : 
    case 147 : {
      push_exception_continuationB(15, 1);
      arg0K0 = 0;
      goto L17320;}
      break;
    case 1 : {
      count_397X = ((((*((unsigned char *) ((Scode_pointerS) + 1))))<<8)) + (*((unsigned char *) ((Scode_pointerS) + 2)));
      *((long *) (SstackS)) = (SenvS);
      SstackS = ((SstackS) + -4);
      *((long *) (SstackS)) = (1034 + (((count_397X)<<10)));
      SstackS = ((SstackS) + -4);
      SenvS = (3 + (((long) ((SstackS) + 8))));
      Scode_pointerS = ((Scode_pointerS) + 3);
      arg1K0 = (Scode_pointerS);
      goto L19093;}
      break;
    case 2 : {
      SvalS = (*((long *) ((((char *) (-3 + (StemplateS)))) + ((((((((*((unsigned char *) ((Scode_pointerS) + 1))))<<8)) + (*((unsigned char *) ((Scode_pointerS) + 2)))))<<2)))));
      Scode_pointerS = ((Scode_pointerS) + 3);
      arg1K0 = (Scode_pointerS);
      goto L19093;}
      break;
    case 3 : {
      SvalS = (*((long *) ((((char *) (-3 + (StemplateS)))) + ((((*((unsigned char *) ((Scode_pointerS) + 1))))<<2)))));
      Scode_pointerS = ((Scode_pointerS) + 3);
      arg1K0 = (Scode_pointerS);
      goto L19093;}
      break;
    case 4 : {
      back_398X = *((unsigned char *) ((Scode_pointerS) + 1));
      env_399X = SenvS;
      arg0K0 = env_399X;
      arg0K1 = back_398X;
      goto L24496;}
      break;
    case 5 : {
      SvalS = (*((long *) ((((char *) (-3 + (SenvS)))) + ((((*((unsigned char *) ((Scode_pointerS) + 1))))<<2)))));
      if ((529 == (SvalS))) {
        push_exception_continuationB(0, 2);
        arg0K0 = 0;
        goto L17320;}
      else {
        Scode_pointerS = ((Scode_pointerS) + 2);
        arg1K0 = (Scode_pointerS);
        goto L19093;}}
      break;
    case 6 : {
      SvalS = (*((long *) ((((char *) (-3 + (*((long *) (((char *) (-3 + (SenvS))))))))) + ((((*((unsigned char *) ((Scode_pointerS) + 1))))<<2)))));
      if ((529 == (SvalS))) {
        push_exception_continuationB(0, 2);
        arg0K0 = 0;
        goto L17320;}
      else {
        Scode_pointerS = ((Scode_pointerS) + 2);
        arg1K0 = (Scode_pointerS);
        goto L19093;}}
      break;
    case 7 : {
      SvalS = (*((long *) ((((char *) (-3 + (*((long *) (((char *) (-3 + (*((long *) (((char *) (-3 + (SenvS)))))))))))))) + ((((*((unsigned char *) ((Scode_pointerS) + 1))))<<2)))));
      if ((529 == (SvalS))) {
        push_exception_continuationB(0, 2);
        arg0K0 = 0;
        goto L17320;}
      else {
        Scode_pointerS = ((Scode_pointerS) + 2);
        arg1K0 = (Scode_pointerS);
        goto L19093;}}
      break;
    case 8 : {
      back_400X = ((((*((unsigned char *) ((Scode_pointerS) + 1))))<<8)) + (*((unsigned char *) ((Scode_pointerS) + 2)));
      env_401X = SenvS;
      arg0K0 = env_401X;
      arg0K1 = back_400X;
      goto L20911;}
      break;
    case 9 : {
      back_402X = ((((*((unsigned char *) ((Scode_pointerS) + 1))))<<8)) + (*((unsigned char *) ((Scode_pointerS) + 2)));
      env_403X = SenvS;
      arg0K0 = env_403X;
      arg0K1 = back_402X;
      goto L20849;}
      break;
    case 10 : {
      location_404X = *((long *) ((((char *) (-3 + (StemplateS)))) + ((((((((*((unsigned char *) ((Scode_pointerS) + 1))))<<8)) + (*((unsigned char *) ((Scode_pointerS) + 2)))))<<2))));
      SvalS = (*((long *) ((((char *) (-3 + location_404X))) + 4)));
      if ((17 == (255 & (SvalS)))) {
        push_exception_continuationB(1, 3);
        *((long *) (SstackS)) = location_404X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 1;
        goto L17320;}
      else {
        Scode_pointerS = ((Scode_pointerS) + 3);
        arg1K0 = (Scode_pointerS);
        goto L19093;}}
      break;
    case 11 : {
      location_405X = *((long *) ((((char *) (-3 + (StemplateS)))) + ((((((((*((unsigned char *) ((Scode_pointerS) + 1))))<<8)) + (*((unsigned char *) ((Scode_pointerS) + 2)))))<<2))));
      if ((273 == (*((long *) ((((char *) (-3 + location_405X))) + 4))))) {
        push_exception_continuationB(1, 3);
        *((long *) (SstackS)) = location_405X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = (SvalS);
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17320;}
      else {
        val_406X = SvalS;
        addr_407X = (((char *) (-3 + location_405X))) + 4;
        S48_WRITE_BARRIER(location_405X, addr_407X, val_406X);
        *((long *) addr_407X) = val_406X;
        SvalS = 13;
        Scode_pointerS = ((Scode_pointerS) + 3);
        arg1K0 = (Scode_pointerS);
        goto L19093;}}
      break;
    case 12 : {
      v_408X = *((unsigned char *) ((Scode_pointerS) + 3));
      if ((0 == v_408X)) {
        p_409X = SenvS;
        if ((3 == (3 & p_409X))) {
          if ((p_409X < (((long) (Sstack_beginS))))) {
            arg0K0 = 0;
            goto L21628;}
          else {
            if (((((long) (Sstack_endS))) < p_409X)) {
              arg0K0 = 0;
              goto L21628;}
            else {
              arg0K0 = ((Sstack_endS) - (Sstack_beginS));
              goto L21628;}}}
        else {
          arg0K0 = 0;
          goto L21628;}}
      else {
        arg0K0 = (SvalS);
        goto L21635;}}
      break;
    case 13 : {
      total_count_410X = *((unsigned char *) ((Scode_pointerS) + 2));
      space_411X = 1 + total_count_410X;
      v_412X = AVAILABLEp(space_411X);
      if (v_412X) {
        goto L21430;}
      else {
        collect_saving_temps(1, 1, &temp1_413X);
        v_414X = AVAILABLEp(space_411X);
        if (v_414X) {
          goto L21430;}
        else {
          ps_error("Scheme48 heap overflow", 0);
          goto L21430;}}}
      break;
    case 14 : {
      *((long *) (SstackS)) = (SvalS);
      SstackS = ((SstackS) + -4);
      Scode_pointerS = ((Scode_pointerS) + 1);
      arg1K0 = (Scode_pointerS);
      goto L19093;}
      break;
    case 15 : {
      SvalS = (*((long *) ((((char *) (-3 + (SenvS)))) + ((((*((unsigned char *) ((Scode_pointerS) + 1))))<<2)))));
      if ((529 == (SvalS))) {
        push_exception_continuationB(0, 3);
        arg0K0 = 0;
        goto L17320;}
      else {
        *((long *) (SstackS)) = (SvalS);
        SstackS = ((SstackS) + -4);
        Scode_pointerS = ((Scode_pointerS) + 3);
        arg1K0 = (Scode_pointerS);
        goto L19093;}}
      break;
    case 16 : {
      *((long *) (SstackS)) = (SvalS);
      SstackS = ((SstackS) + -4);
      SvalS = (*((long *) ((((char *) (-3 + (SenvS)))) + ((((*((unsigned char *) ((Scode_pointerS) + 2))))<<2)))));
      if ((529 == (SvalS))) {
        push_exception_continuationB(0, 3);
        arg0K0 = 0;
        goto L17320;}
      else {
        Scode_pointerS = ((Scode_pointerS) + 3);
        arg1K0 = (Scode_pointerS);
        goto L19093;}}
      break;
    case 17 : {
      SstackS = ((SstackS) + 4);
      SvalS = (*((long *) (SstackS)));
      Scode_pointerS = ((Scode_pointerS) + 1);
      arg1K0 = (Scode_pointerS);
      goto L19093;}
      break;
    case 18 : {
      SvalS = (*((long *) ((SstackS) + (4 + ((((*((unsigned char *) ((Scode_pointerS) + 1))))<<2))))));
      Scode_pointerS = ((Scode_pointerS) + 2);
      arg1K0 = (Scode_pointerS);
      goto L19093;}
      break;
    case 19 : {
      *((long *) ((SstackS) + (4 + ((((*((unsigned char *) ((Scode_pointerS) + 1))))<<2))))) = (SvalS);
      Scode_pointerS = ((Scode_pointerS) + 2);
      arg1K0 = (Scode_pointerS);
      goto L19093;}
      break;
    case 20 : {
      push_continuationB(((Scode_pointerS) + (((((*((unsigned char *) ((Scode_pointerS) + 1))))<<8)) + (*((unsigned char *) ((Scode_pointerS) + 2))))), (*((unsigned char *) ((Scode_pointerS) + 3))));
      Scode_pointerS = ((Scode_pointerS) + 4);
      arg1K0 = (Scode_pointerS);
      goto L19093;}
      break;
    case 21 : {
      push_continuationB(((Scode_pointerS) + (((((*((unsigned char *) ((Scode_pointerS) + 1))))<<8)) + (*((unsigned char *) ((Scode_pointerS) + 2))))), (((((*((unsigned char *) ((Scode_pointerS) + 3))))<<8)) + (*((unsigned char *) ((Scode_pointerS) + 4)))));
      Scode_pointerS = ((Scode_pointerS) + 5);
      arg1K0 = (Scode_pointerS);
      goto L19093;}
      break;
    case 22 : {
      space_415X = 1 + (((((Sstack_endS) - (SstackS)))>>2));
      v_416X = AVAILABLEp(space_415X);
      if (v_416X) {
        arg2K0 = 1;
        arg0K1 = 0;
        goto L21356;}
      else {
        collect_saving_temps(1, 1, &temp1_417X);
        v_418X = AVAILABLEp(space_415X);
        if (v_418X) {
          arg2K0 = 1;
          arg0K1 = 0;
          goto L21356;}
        else {
          arg2K0 = 0;
          arg0K1 = 0;
          goto L21356;}}}
      break;
    case 23 : {
      cont_419X = *((long *) (((char *) (-3 + (Sbottom_of_stackS)))));
      if ((3 == (3 & cont_419X))) {
        if ((10 == (31 & ((((*((long *) ((((char *) (-3 + cont_419X))) + -4))))>>2))))) {
          if ((3 == (3 & cont_419X))) {
            if ((10 == (31 & ((((*((long *) ((((char *) (-3 + cont_419X))) + -4))))>>2))))) {
              merged_arg0K0 = cont_419X;
              copy_continuation_from_heapB_return_tag = 0;
              goto copy_continuation_from_heapB;
             copy_continuation_from_heapB_return_0:
              goto L20502;}
            else {
              goto L20539;}}
          else {
            goto L20539;}}
        else {
          goto L20503;}}
      else {
        goto L20503;}}
      break;
    case 24 : {
      stack_arg_count_420X = *((unsigned char *) ((Scode_pointerS) + 1));
      obj_421X = SvalS;
      if ((3 == (3 & obj_421X))) {
        if ((3 == (31 & ((((*((long *) ((((char *) (-3 + obj_421X))) + -4))))>>2))))) {
          template_422X = *((long *) (((char *) (-3 + (SvalS)))));
          code_423X = *((long *) (((char *) (-3 + template_422X))));
          if ((stack_arg_count_420X == (*((unsigned char *) ((((char *) (-3 + code_423X))) + 1))))) {
            StemplateS = template_422X;
            SenvS = (*((long *) ((((char *) (-3 + (SvalS)))) + 4)));
            Scode_pointerS = ((((char *) (-3 + code_423X))) + 2);
            if (((SstackS) < (Sstack_limitS))) {
              space_424X = 1 + (((((Sstack_endS) - (SstackS)))>>2));
              v_425X = AVAILABLEp(space_424X);
              if (v_425X) {
                arg2K0 = 1;
                arg0K1 = 0;
                goto L19863;}
              else {
                collect_saving_temps(1, 1, &temp1_426X);
                v_427X = AVAILABLEp(space_424X);
                if (v_427X) {
                  arg2K0 = 1;
                  arg0K1 = 0;
                  goto L19863;}
                else {
                  arg2K0 = 0;
                  arg0K1 = 0;
                  goto L19863;}}}
            else {
              goto L19775;}}
          else {
            arg0K0 = stack_arg_count_420X;
            goto L16721;}}
        else {
          arg0K0 = 3;
          arg0K1 = stack_arg_count_420X;
          arg0K2 = 25;
          arg0K3 = 0;
          goto L17816;}}
      else {
        arg0K0 = 3;
        arg0K1 = stack_arg_count_420X;
        arg0K2 = 25;
        arg0K3 = 0;
        goto L17816;}}
      break;
    case 25 : {
      stack_arg_count_428X = ((((*((unsigned char *) ((Scode_pointerS) + 1))))<<8)) + (*((unsigned char *) ((Scode_pointerS) + 2)));
      obj_429X = SvalS;
      if ((3 == (3 & obj_429X))) {
        if ((3 == (31 & ((((*((long *) ((((char *) (-3 + obj_429X))) + -4))))>>2))))) {
          arg0K0 = stack_arg_count_428X;
          goto L16721;}
        else {
          arg0K0 = 3;
          arg0K1 = stack_arg_count_428X;
          arg0K2 = 25;
          arg0K3 = 0;
          goto L17816;}}
      else {
        arg0K0 = 3;
        arg0K1 = stack_arg_count_428X;
        arg0K2 = 25;
        arg0K3 = 0;
        goto L17816;}}
      break;
    case 26 : {
      nargs_430X = *((unsigned char *) ((Scode_pointerS) + 1));
      top_of_args_431X = SstackS;
      stob_432X = ScontS;
      arg1K0 = ((((char *) (-3 + stob_432X))) + -8);
      arg1K1 = (top_of_args_431X + (((nargs_430X)<<2)));
      goto L18283;}
      break;
    case 27 : {
      SstackS = ((SstackS) + 4);
      list_args_433X = *((long *) (SstackS));
      merged_arg0K0 = list_args_433X;
      okay_argument_list_return_tag = 0;
      goto okay_argument_list;
     okay_argument_list_return_0:
      okayP_434X = okay_argument_list0_return_value;
      length_435X = okay_argument_list1_return_value;
      if (okayP_434X) {
        arg0K0 = (((((*((unsigned char *) ((Scode_pointerS) + 1))))<<8)) + (*((unsigned char *) ((Scode_pointerS) + 2))));
        arg0K1 = list_args_433X;
        arg0K2 = length_435X;
        goto L18321;}
      else {
        *((long *) (SstackS)) = list_args_433X;
        SstackS = ((SstackS) + -4);
        merged_arg0K0 = 25;
        merged_arg0K1 = (1 + (((((*((unsigned char *) ((Scode_pointerS) + 1))))<<8)) + (*((unsigned char *) ((Scode_pointerS) + 2)))));
        pop_args_GlistS_return_tag = 0;
        goto pop_args_GlistS;
       pop_args_GlistS_return_0:
        args_436X = pop_args_GlistS0_return_value;
        push_exception_continuationB(5, 0);
        *((long *) (SstackS)) = (SvalS);
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = args_436X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17320;}}
      break;
    case 28 : {
      SstackS = ((SstackS) + 4);
      SstackS = ((SstackS) + 4);
      stack_nargs_437X = (((*((long *) (SstackS))))>>2);
      proc_438X = *((long *) ((SstackS) + (4 + (((stack_nargs_437X)<<2)))));
      *((long *) ((SstackS) + (4 + (((stack_nargs_437X)<<2))))) = 1;
      SstackS = ((SstackS) + 4);
      rest_list_439X = *((long *) (SstackS));
      if ((25 == rest_list_439X)) {
        SstackS = ((SstackS) + 4);
        arg0K0 = (*((long *) (SstackS)));
        arg0K1 = (-2 + stack_nargs_437X);
        goto L11915;}
      else {
        if ((25 == (*((long *) ((((char *) (-3 + rest_list_439X))) + 4))))) {
          arg0K0 = (*((long *) (((char *) (-3 + rest_list_439X)))));
          arg0K1 = (-1 + stack_nargs_437X);
          goto L11915;}
        else {
          arg0K0 = (*((long *) ((((char *) (-3 + (*((long *) ((((char *) (-3 + rest_list_439X))) + 4)))))) + 4)));
          arg0K1 = rest_list_439X;
          goto L11956;}}}
      break;
    case 29 : {
      SstackS = ((SstackS) + 4);
      cont_440X = *((long *) (SstackS));
      if ((3 == (3 & cont_440X))) {
        if ((10 == (31 & ((((*((long *) ((((char *) (-3 + cont_440X))) + -4))))>>2))))) {
          merged_arg0K0 = cont_440X;
          copy_continuation_from_heapB_return_tag = 1;
          goto copy_continuation_from_heapB;
         copy_continuation_from_heapB_return_1:
          goto L18226;}
        else {
          goto L18233;}}
      else {
        goto L18233;}}
      break;
    case 30 : {
      SstackS = ((SstackS) + 4);
      consumer_441X = *((long *) (SstackS));
      *((long *) (SstackS)) = (SvalS);
      SstackS = ((SstackS) + -4);
      SvalS = consumer_441X;
      obj_442X = SvalS;
      if ((3 == (3 & obj_442X))) {
        if ((3 == (31 & ((((*((long *) ((((char *) (-3 + obj_442X))) + -4))))>>2))))) {
          arg0K0 = 1;
          goto L16721;}
        else {
          arg0K0 = 3;
          arg0K1 = 1;
          arg0K2 = 25;
          arg0K3 = 0;
          goto L17816;}}
      else {
        arg0K0 = 3;
        arg0K1 = 1;
        arg0K2 = 25;
        arg0K3 = 0;
        goto L17816;}}
      break;
    case 31 : {
      pop_continuationB_return_tag = 0;
      goto pop_continuationB;
     pop_continuationB_return_0:
      arg1K0 = (Scode_pointerS);
      goto L19093;}
      break;
    case 32 : {
      arg0K0 = (((((*((unsigned char *) ((Scode_pointerS) + 1))))<<8)) + (*((unsigned char *) ((Scode_pointerS) + 2))));
      arg0K1 = 25;
      arg0K2 = 0;
      goto L21091;}
      break;
    case 33 : {
      SstackS = ((SstackS) + 4);
      p_443X = *((long *) (SstackS));
      SstackS = ((SstackS) + 4);
      stack_nargs_444X = (((*((long *) (SstackS))))>>2);
      SstackS = ((SstackS) + 4);
      arg0K0 = stack_nargs_444X;
      arg0K1 = (*((long *) (SstackS)));
      arg0K2 = ((((p_443X)>>2)) - stack_nargs_444X);
      goto L21091;}
      break;
    case 34 : {
      Scode_pointerS = ((Scode_pointerS) + 1);
      arg1K0 = (Scode_pointerS);
      goto L19093;}
      break;
    case 35 : {
      tem_445X = *((long *) ((((char *) (-3 + (StemplateS)))) + ((((((((*((unsigned char *) ((Scode_pointerS) + 1))))<<8)) + (*((unsigned char *) ((Scode_pointerS) + 2)))))<<2))));
      StemplateS = tem_445X;
      Scode_pointerS = (((char *) (-3 + (*((long *) (((char *) (-3 + tem_445X))))))));
      arg1K0 = (Scode_pointerS);
      goto L19093;}
      break;
    case 36 : {
      code_446X = *((long *) (((char *) (-3 + (*((long *) ((((char *) (-3 + (StemplateS)))) + ((((((((*((unsigned char *) ((Scode_pointerS) + 1))))<<8)) + (*((unsigned char *) ((Scode_pointerS) + 2)))))<<2)))))))));
      if ((0 == (*((unsigned char *) ((((char *) (-3 + code_446X))) + 1))))) {
        if (((SstackS) < (Sstack_limitS))) {
          space_447X = 1 + (((((Sstack_endS) - (SstackS)))>>2));
          v_448X = AVAILABLEp(space_447X);
          if (v_448X) {
            arg2K0 = 1;
            arg0K1 = 0;
            goto L19599;}
          else {
            collect_saving_temps(1, 1, &temp1_449X);
            v_450X = AVAILABLEp(space_447X);
            if (v_450X) {
              arg2K0 = 1;
              arg0K1 = 0;
              goto L19599;}
            else {
              arg2K0 = 0;
              arg0K1 = 0;
              goto L19599;}}}
        else {
          goto L19472;}}
      else {
        v_451X = *((unsigned char *) ((((char *) (-3 + code_446X))) + 1));
        if ((66 == v_451X)) {
          if ((0 == (*((unsigned char *) ((((char *) (-3 + code_446X))) + (-3 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + code_446X))) + -4))))>>8)))))))) {
            index_452X = -2 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + code_446X))) + -4))))>>8));
            space_453X = ((((*((unsigned char *) ((((char *) (-3 + code_446X))) + index_452X))))<<8)) + (*((unsigned char *) ((((char *) (-3 + code_446X))) + (1 + index_452X))));
            if ((space_453X < (64 + (((((SstackS) - (Sstack_limitS)))>>2))))) {
              goto L19472;}
            else {
              space_454X = 1 + (((((Sstack_endS) - (SstackS)))>>2));
              v_455X = AVAILABLEp(space_454X);
              if (v_455X) {
                arg2K0 = 1;
                arg0K1 = 0;
                goto L19655;}
              else {
                collect_saving_temps(1, 1, &temp1_456X);
                v_457X = AVAILABLEp(space_454X);
                if (v_457X) {
                  arg2K0 = 1;
                  arg0K1 = 0;
                  goto L19655;}
                else {
                  arg2K0 = 0;
                  arg0K1 = 0;
                  goto L19655;}}}}
          else {
            goto L19517;}}
        else {
          goto L19517;}}}
      break;
    case 37 : {
      if ((1 == (SvalS))) {
        Scode_pointerS = ((Scode_pointerS) + (((((*((unsigned char *) ((Scode_pointerS) + 1))))<<8)) + (*((unsigned char *) ((Scode_pointerS) + 2)))));
        arg1K0 = (Scode_pointerS);
        goto L19093;}
      else {
        Scode_pointerS = ((Scode_pointerS) + 3);
        arg1K0 = (Scode_pointerS);
        goto L19093;}}
      break;
    case 38 : {
      Scode_pointerS = ((Scode_pointerS) + (((((*((unsigned char *) ((Scode_pointerS) + 1))))<<8)) + (*((unsigned char *) ((Scode_pointerS) + 2)))));
      arg1K0 = (Scode_pointerS);
      goto L19093;}
      break;
    case 39 : {
      if ((0 == (3 & (SvalS)))) {
        p_458X = SvalS;
        max_459X = *((unsigned char *) ((Scode_pointerS) + 1));
        val_460X = ((p_458X)>>2);
        if ((val_460X < 0)) {
          goto L19373;}
        else {
          if ((val_460X < max_459X)) {
            index_461X = 1 + (((val_460X)<<1));
            arg0K0 = (((((*((unsigned char *) ((Scode_pointerS) + (1 + index_461X)))))<<8)) + (*((unsigned char *) ((Scode_pointerS) + (2 + index_461X)))));
            goto L19375;}
          else {
            goto L19373;}}}
      else {
        push_exception_continuationB(5, 0);
        *((long *) (SstackS)) = (SvalS);
        SstackS = ((SstackS) + -4);
        arg0K0 = 1;
        goto L17320;}}
      break;
    case 40 : {
      stack_nargs_462X = (((*((long *) ((SstackS) + 4))))>>2);
      if ((0 == stack_nargs_462X)) {
        arg0_463X = *((long *) ((SstackS) + 12));
        rest_list_464X = *((long *) ((SstackS) + 8));
        *((long *) (SstackS)) = arg0_463X;
        SstackS = ((SstackS) + -4);
        SvalS = (*((long *) (((char *) (-3 + rest_list_464X)))));
        goto L20405;}
      else {
        arg1_465X = *((long *) ((SstackS) + (4 + (((stack_nargs_462X)<<2)))));
        arg0_466X = *((long *) ((SstackS) + (8 + (((stack_nargs_462X)<<2)))));
        *((long *) ((SstackS) + (8 + (((stack_nargs_462X)<<2))))) = 1;
        *((long *) ((SstackS) + 4)) = (-4 + (((stack_nargs_462X)<<2)));
        *((long *) (SstackS)) = arg0_466X;
        SstackS = ((SstackS) + -4);
        SvalS = arg1_465X;
        goto L20405;}}
      break;
    case 41 : {
      stack_nargs_467X = (((*((long *) ((SstackS) + 4))))>>2);
      if ((stack_nargs_467X == 0)) {
        rest_list_468X = *((long *) ((SstackS) + 8));
        if ((25 == (*((long *) ((((char *) (-3 + rest_list_468X))) + 4))))) {
          arg0K0 = 1;
          goto L19271;}
        else {
          *((long *) ((SstackS) + 8)) = (*((long *) ((((char *) (-3 + rest_list_468X))) + 4)));
          *((long *) ((SstackS) + 12)) = (SvalS);
          arg0K0 = -2;
          goto L19271;}}
      else {
        if ((stack_nargs_467X == 1)) {
          if ((25 == (*((long *) ((SstackS) + 8))))) {
            arg0K0 = 1;
            goto L19271;}
          else {
            *((long *) ((SstackS) + 4)) = 0;
            *((long *) ((SstackS) + 12)) = (SvalS);
            arg0K0 = -2;
            goto L19271;}}
        else {
          *((long *) ((SstackS) + (8 + (((stack_nargs_467X)<<2))))) = (SvalS);
          arg0K0 = -2;
          goto L19271;}}}
      break;
    case 42 : {
      if ((1 == (SvalS))) {
        Scode_pointerS = ((Scode_pointerS) + 1);
        arg1K0 = (Scode_pointerS);
        goto L19093;}
      else {
        x_469X = (((*((long *) ((SstackS) + 4))))>>2);
        if ((x_469X == 0)) {
          rest_list_470X = *((long *) ((SstackS) + 8));
          if ((25 == (*((long *) ((((char *) (-3 + rest_list_470X))) + 4))))) {
            arg0K0 = 1;
            goto L20296;}
          else {
            *((long *) ((SstackS) + 8)) = (*((long *) ((((char *) (-3 + rest_list_470X))) + 4)));
            *((long *) ((SstackS) + 12)) = (*((long *) (((char *) (-3 + rest_list_470X)))));
            arg0K0 = -2;
            goto L20296;}}
        else {
          if ((x_469X == 1)) {
            if ((25 == (*((long *) ((SstackS) + 8))))) {
              arg0K0 = 1;
              goto L20296;}
            else {
              *((long *) ((SstackS) + 4)) = 0;
              arg0K0 = -2;
              goto L20296;}}
          else {
            arg0K0 = -2;
            goto L20296;}}}}
      break;
    case 43 : {
      SstackS = ((SstackS) + 4);
      arg2_471X = *((long *) (SstackS));
      x_472X = SvalS;
      if ((arg2_471X == x_472X)) {
        arg0K0 = 5;
        goto L39115;}
      else {
        arg0K0 = 1;
        goto L39115;}}
      break;
    case 44 : {
      x_473X = SvalS;
      if ((0 == (3 & x_473X))) {
        arg0K0 = 5;
        goto L39127;}
      else {
        if ((3 == (3 & x_473X))) {
          if ((20 == (31 & ((((*((long *) ((((char *) (-3 + x_473X))) + -4))))>>2))))) {
            arg0K0 = 5;
            goto L39127;}
          else {
            goto L11262;}}
        else {
          goto L11262;}}}
      break;
    case 45 : {
      n_474X = SvalS;
      if ((0 == (3 & n_474X))) {
        goto L29202;}
      else {
        if ((3 == (3 & n_474X))) {
          if ((20 == (31 & ((((*((long *) ((((char *) (-3 + n_474X))) + -4))))>>2))))) {
            goto L29202;}
          else {
            goto L29203;}}
        else {
          goto L29203;}}}
      break;
    case 46 : {
      n_475X = SvalS;
      if ((0 == (3 & n_475X))) {
        goto L29359;}
      else {
        if ((3 == (3 & n_475X))) {
          if ((20 == (31 & ((((*((long *) ((((char *) (-3 + n_475X))) + -4))))>>2))))) {
            goto L29359;}
          else {
            goto L29334;}}
        else {
          goto L29334;}}}
      break;
    case 47 : {
      n_476X = SvalS;
      if ((0 == (3 & n_476X))) {
        goto L29558;}
      else {
        if ((3 == (3 & n_476X))) {
          if ((20 == (31 & ((((*((long *) ((((char *) (-3 + n_476X))) + -4))))>>2))))) {
            goto L29558;}
          else {
            goto L29533;}}
        else {
          goto L29533;}}}
      break;
    case 48 : {
      n_477X = SvalS;
      if ((0 == (3 & n_477X))) {
        goto L29757;}
      else {
        if ((3 == (3 & n_477X))) {
          if ((20 == (31 & ((((*((long *) ((((char *) (-3 + n_477X))) + -4))))>>2))))) {
            goto L29757;}
          else {
            goto L29732;}}
        else {
          goto L29732;}}}
      break;
    case 49 : {
      x_478X = SvalS;
      if ((0 == (3 & x_478X))) {
        SvalS = 5;
        Scode_pointerS = ((Scode_pointerS) + 1);
        arg1K0 = (Scode_pointerS);
        goto L19093;}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = x_478X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 1;
        goto L17320;}}
      break;
    case 50 : {
      x_479X = SvalS;
      push_exception_continuationB(5, 1);
      *((long *) (SstackS)) = x_479X;
      SstackS = ((SstackS) + -4);
      arg0K0 = 1;
      goto L17320;}
      break;
    case 51 : {
      x_480X = SvalS;
      push_exception_continuationB(5, 1);
      *((long *) (SstackS)) = x_480X;
      SstackS = ((SstackS) + -4);
      arg0K0 = 1;
      goto L17320;}
      break;
    case 52 : {
      SstackS = ((SstackS) + 4);
      x_481X = *((long *) (SstackS));
      y_482X = SvalS;
      if ((0 == (3 & (x_481X | y_482X)))) {
        z_483X = (((x_481X)>>2)) + (((y_482X)>>2));
        if ((536870911 < z_483X)) {
          push_exception_continuationB(5, 1);
          *((long *) (SstackS)) = x_481X;
          SstackS = ((SstackS) + -4);
          *((long *) (SstackS)) = y_482X;
          SstackS = ((SstackS) + -4);
          arg0K0 = 2;
          goto L17320;}
        else {
          if ((z_483X < -536870912)) {
            push_exception_continuationB(5, 1);
            *((long *) (SstackS)) = x_481X;
            SstackS = ((SstackS) + -4);
            *((long *) (SstackS)) = y_482X;
            SstackS = ((SstackS) + -4);
            arg0K0 = 2;
            goto L17320;}
          else {
            SvalS = (((z_483X)<<2));
            Scode_pointerS = ((Scode_pointerS) + 1);
            arg1K0 = (Scode_pointerS);
            goto L19093;}}}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = x_481X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = y_482X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17320;}}
      break;
    case 53 : {
      SstackS = ((SstackS) + 4);
      arg2_484X = *((long *) (SstackS));
      x_485X = SvalS;
      if ((0 == (3 & (arg2_484X | x_485X)))) {
        a_486X = ((arg2_484X)>>2);
        b_487X = ((x_485X)>>2);
        if ((a_486X < 0)) {
          arg0K0 = (0 - a_486X);
          goto L6998;}
        else {
          arg0K0 = a_486X;
          goto L6998;}}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = arg2_484X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = x_485X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17320;}}
      break;
    case 54 : {
      SstackS = ((SstackS) + 4);
      x_488X = *((long *) (SstackS));
      y_489X = SvalS;
      if ((0 == (3 & (x_488X | y_489X)))) {
        z_490X = (((x_488X)>>2)) - (((y_489X)>>2));
        if ((536870911 < z_490X)) {
          push_exception_continuationB(5, 1);
          *((long *) (SstackS)) = x_488X;
          SstackS = ((SstackS) + -4);
          *((long *) (SstackS)) = y_489X;
          SstackS = ((SstackS) + -4);
          arg0K0 = 2;
          goto L17320;}
        else {
          if ((z_490X < -536870912)) {
            push_exception_continuationB(5, 1);
            *((long *) (SstackS)) = x_488X;
            SstackS = ((SstackS) + -4);
            *((long *) (SstackS)) = y_489X;
            SstackS = ((SstackS) + -4);
            arg0K0 = 2;
            goto L17320;}
          else {
            SvalS = (((z_490X)<<2));
            Scode_pointerS = ((Scode_pointerS) + 1);
            arg1K0 = (Scode_pointerS);
            goto L19093;}}}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = x_488X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = y_489X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17320;}}
      break;
    case 55 : {
      SstackS = ((SstackS) + 4);
      arg2_491X = *((long *) (SstackS));
      x_492X = SvalS;
      if ((0 == (3 & (arg2_491X | x_492X)))) {
        if ((0 == x_492X)) {
          push_exception_continuationB(5, 1);
          *((long *) (SstackS)) = arg2_491X;
          SstackS = ((SstackS) + -4);
          *((long *) (SstackS)) = x_492X;
          SstackS = ((SstackS) + -4);
          arg0K0 = 2;
          goto L17320;}
        else {
          a_493X = ((arg2_491X)>>2);
          b_494X = ((x_492X)>>2);
          if ((a_493X < 0)) {
            arg0K0 = (0 - a_493X);
            goto L7279;}
          else {
            arg0K0 = a_493X;
            goto L7279;}}}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = arg2_491X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = x_492X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17320;}}
      break;
    case 56 : {
      SstackS = ((SstackS) + 4);
      arg2_495X = *((long *) (SstackS));
      x_496X = SvalS;
      if ((0 == (3 & (arg2_495X | x_496X)))) {
        if ((arg2_495X == x_496X)) {
          arg0K0 = 5;
          goto L32718;}
        else {
          arg0K0 = 1;
          goto L32718;}}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = arg2_495X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = x_496X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17320;}}
      break;
    case 57 : {
      SstackS = ((SstackS) + 4);
      arg2_497X = *((long *) (SstackS));
      x_498X = SvalS;
      if ((0 == (3 & (arg2_497X | x_498X)))) {
        if ((arg2_497X < x_498X)) {
          arg0K0 = 5;
          goto L32796;}
        else {
          arg0K0 = 1;
          goto L32796;}}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = arg2_497X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = x_498X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17320;}}
      break;
    case 58 : {
      SstackS = ((SstackS) + 4);
      arg2_499X = *((long *) (SstackS));
      x_500X = SvalS;
      if ((0 == (3 & (arg2_499X | x_500X)))) {
        if ((x_500X < arg2_499X)) {
          arg0K0 = 5;
          goto L32874;}
        else {
          arg0K0 = 1;
          goto L32874;}}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = arg2_499X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = x_500X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17320;}}
      break;
    case 59 : {
      SstackS = ((SstackS) + 4);
      arg2_501X = *((long *) (SstackS));
      x_502X = SvalS;
      if ((0 == (3 & (arg2_501X | x_502X)))) {
        if ((x_502X < arg2_501X)) {
          arg0K0 = 1;
          goto L32952;}
        else {
          arg0K0 = 5;
          goto L32952;}}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = arg2_501X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = x_502X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17320;}}
      break;
    case 60 : {
      SstackS = ((SstackS) + 4);
      arg2_503X = *((long *) (SstackS));
      x_504X = SvalS;
      if ((0 == (3 & (arg2_503X | x_504X)))) {
        if ((arg2_503X < x_504X)) {
          arg0K0 = 1;
          goto L33030;}
        else {
          arg0K0 = 5;
          goto L33030;}}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = arg2_503X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = x_504X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17320;}}
      break;
    case 61 : {
      SstackS = ((SstackS) + 4);
      arg2_505X = *((long *) (SstackS));
      x_506X = SvalS;
      if ((0 == (3 & (arg2_505X | x_506X)))) {
        if ((0 == x_506X)) {
          push_exception_continuationB(5, 1);
          *((long *) (SstackS)) = arg2_505X;
          SstackS = ((SstackS) + -4);
          *((long *) (SstackS)) = x_506X;
          SstackS = ((SstackS) + -4);
          arg0K0 = 2;
          goto L17320;}
        else {
          a_507X = ((arg2_505X)>>2);
          b_508X = ((x_506X)>>2);
          if ((a_507X < 0)) {
            arg0K0 = (0 - a_507X);
            goto L7456;}
          else {
            arg0K0 = a_507X;
            goto L7456;}}}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = arg2_505X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = x_506X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17320;}}
      break;
    case 62 : {
      SstackS = ((SstackS) + 4);
      x_509X = *((long *) (SstackS));
      y_510X = SvalS;
      if ((0 == (3 & (x_509X | y_510X)))) {
        if ((0 == y_510X)) {
          push_exception_continuationB(5, 1);
          *((long *) (SstackS)) = x_509X;
          SstackS = ((SstackS) + -4);
          *((long *) (SstackS)) = y_510X;
          SstackS = ((SstackS) + -4);
          arg0K0 = 2;
          goto L17320;}
        else {
          a_511X = ((x_509X)>>2);
          if ((a_511X < 0)) {
            arg0K0 = (0 - a_511X);
            goto L26010;}
          else {
            arg0K0 = a_511X;
            goto L26010;}}}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = x_509X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = y_510X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17320;}}
      break;
    case 63 : {
      x_512X = SvalS;
      if ((0 == (3 & x_512X))) {
        SvalS = x_512X;
        Scode_pointerS = ((Scode_pointerS) + 1);
        arg1K0 = (Scode_pointerS);
        goto L19093;}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = x_512X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 1;
        goto L17320;}}
      break;
    case 64 : {
      x_513X = SvalS;
      if ((0 == (3 & x_513X))) {
        SvalS = x_513X;
        Scode_pointerS = ((Scode_pointerS) + 1);
        arg1K0 = (Scode_pointerS);
        goto L19093;}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = x_513X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 1;
        goto L17320;}}
      break;
    case 65 : {
      x_514X = SvalS;
      if ((0 == (3 & x_514X))) {
        SvalS = 4;
        Scode_pointerS = ((Scode_pointerS) + 1);
        arg1K0 = (Scode_pointerS);
        goto L19093;}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = x_514X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 1;
        goto L17320;}}
      break;
    case 66 : {
      x_515X = SvalS;
      if ((0 == (3 & x_515X))) {
        SvalS = x_515X;
        Scode_pointerS = ((Scode_pointerS) + 1);
        arg1K0 = (Scode_pointerS);
        goto L19093;}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = x_515X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 1;
        goto L17320;}}
      break;
    case 67 : {
      x_516X = SvalS;
      if ((0 == (3 & x_516X))) {
        SvalS = 0;
        Scode_pointerS = ((Scode_pointerS) + 1);
        arg1K0 = (Scode_pointerS);
        goto L19093;}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = x_516X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 1;
        goto L17320;}}
      break;
    case 68 : {
      x_517X = SvalS;
      push_exception_continuationB(5, 1);
      *((long *) (SstackS)) = x_517X;
      SstackS = ((SstackS) + -4);
      arg0K0 = 1;
      goto L17320;}
      break;
    case 69 : {
      x_518X = SvalS;
      push_exception_continuationB(5, 1);
      *((long *) (SstackS)) = x_518X;
      SstackS = ((SstackS) + -4);
      arg0K0 = 1;
      goto L17320;}
      break;
    case 70 : {
      x_519X = SvalS;
      push_exception_continuationB(5, 1);
      *((long *) (SstackS)) = x_519X;
      SstackS = ((SstackS) + -4);
      arg0K0 = 1;
      goto L17320;}
      break;
    case 71 : {
      x_520X = SvalS;
      push_exception_continuationB(5, 1);
      *((long *) (SstackS)) = x_520X;
      SstackS = ((SstackS) + -4);
      arg0K0 = 1;
      goto L17320;}
      break;
    case 72 : {
      x_521X = SvalS;
      push_exception_continuationB(5, 1);
      *((long *) (SstackS)) = x_521X;
      SstackS = ((SstackS) + -4);
      arg0K0 = 1;
      goto L17320;}
      break;
    case 73 : {
      x_522X = SvalS;
      push_exception_continuationB(5, 1);
      *((long *) (SstackS)) = x_522X;
      SstackS = ((SstackS) + -4);
      arg0K0 = 1;
      goto L17320;}
      break;
    case 74 : {
      x_523X = SvalS;
      push_exception_continuationB(5, 1);
      *((long *) (SstackS)) = x_523X;
      SstackS = ((SstackS) + -4);
      arg0K0 = 1;
      goto L17320;}
      break;
    case 75 : {
      x_524X = SvalS;
      push_exception_continuationB(5, 1);
      *((long *) (SstackS)) = x_524X;
      SstackS = ((SstackS) + -4);
      arg0K0 = 1;
      goto L17320;}
      break;
    case 76 : {
      x_525X = SvalS;
      if ((0 == (3 & x_525X))) {
        if ((x_525X < 0)) {
          goto L39384;}
        else {
          SvalS = 0;
          Scode_pointerS = ((Scode_pointerS) + 1);
          arg1K0 = (Scode_pointerS);
          goto L19093;}}
      else {
        goto L39384;}}
      break;
    case 77 : {
      n_526X = SvalS;
      if ((0 == (3 & n_526X))) {
        n_527X = ((n_526X)>>2);
        if ((n_527X < 0)) {
          arg0K0 = (0 - n_527X);
          goto L26296;}
        else {
          arg0K0 = n_527X;
          goto L26296;}}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = n_526X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 1;
        goto L17320;}}
      break;
    case 78 : {
      SstackS = ((SstackS) + 4);
      arg2_528X = *((long *) (SstackS));
      x_529X = SvalS;
      push_exception_continuationB(5, 1);
      *((long *) (SstackS)) = arg2_528X;
      SstackS = ((SstackS) + -4);
      *((long *) (SstackS)) = x_529X;
      SstackS = ((SstackS) + -4);
      arg0K0 = 2;
      goto L17320;}
      break;
    case 79 : {
      SstackS = ((SstackS) + 4);
      arg2_530X = *((long *) (SstackS));
      x_531X = SvalS;
      push_exception_continuationB(5, 1);
      *((long *) (SstackS)) = arg2_530X;
      SstackS = ((SstackS) + -4);
      *((long *) (SstackS)) = x_531X;
      SstackS = ((SstackS) + -4);
      arg0K0 = 2;
      goto L17320;}
      break;
    case 80 : {
      SstackS = ((SstackS) + 4);
      arg2_532X = *((long *) (SstackS));
      x_533X = SvalS;
      push_exception_continuationB(5, 1);
      *((long *) (SstackS)) = arg2_532X;
      SstackS = ((SstackS) + -4);
      *((long *) (SstackS)) = x_533X;
      SstackS = ((SstackS) + -4);
      arg0K0 = 2;
      goto L17320;}
      break;
    case 81 : {
      x_534X = SvalS;
      if ((0 == (3 & x_534X))) {
        SvalS = (~ (3 | x_534X));
        Scode_pointerS = ((Scode_pointerS) + 1);
        arg1K0 = (Scode_pointerS);
        goto L19093;}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = x_534X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 1;
        goto L17320;}}
      break;
    case 82 : {
      SstackS = ((SstackS) + 4);
      arg2_535X = *((long *) (SstackS));
      x_536X = SvalS;
      if ((0 == (3 & (arg2_535X | x_536X)))) {
        SvalS = (arg2_535X & x_536X);
        Scode_pointerS = ((Scode_pointerS) + 1);
        arg1K0 = (Scode_pointerS);
        goto L19093;}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = arg2_535X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = x_536X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17320;}}
      break;
    case 83 : {
      SstackS = ((SstackS) + 4);
      arg2_537X = *((long *) (SstackS));
      x_538X = SvalS;
      if ((0 == (3 & (arg2_537X | x_538X)))) {
        SvalS = (arg2_537X | x_538X);
        Scode_pointerS = ((Scode_pointerS) + 1);
        arg1K0 = (Scode_pointerS);
        goto L19093;}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = arg2_537X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = x_538X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17320;}}
      break;
    case 84 : {
      SstackS = ((SstackS) + 4);
      arg2_539X = *((long *) (SstackS));
      x_540X = SvalS;
      if ((0 == (3 & (arg2_539X | x_540X)))) {
        SvalS = (arg2_539X ^ x_540X);
        Scode_pointerS = ((Scode_pointerS) + 1);
        arg1K0 = (Scode_pointerS);
        goto L19093;}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = arg2_539X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = x_540X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17320;}}
      break;
    case 85 : {
      SstackS = ((SstackS) + 4);
      x_541X = *((long *) (SstackS));
      y_542X = SvalS;
      if ((0 == (3 & (x_541X | y_542X)))) {
        value_543X = ((x_541X)>>2);
        count_544X = ((y_542X)>>2);
        if ((count_544X < 0)) {
          PS_SHIFT_RIGHT(value_543X, (0 - count_544X), x_545X)
          SvalS = (((x_545X)<<2));
          Scode_pointerS = ((Scode_pointerS) + 1);
          arg1K0 = (Scode_pointerS);
          goto L19093;}
        else {
          PS_SHIFT_LEFT(value_543X, count_544X, x_546X)
          result_547X = (((((x_546X)<<2)))>>2);
          PS_SHIFT_RIGHT(result_547X, count_544X, x_548X)
          if ((value_543X == x_548X)) {
            if ((value_543X < 0)) {
              if ((result_547X < 0)) {
                goto L26575;}
              else {
                push_exception_continuationB(5, 1);
                *((long *) (SstackS)) = x_541X;
                SstackS = ((SstackS) + -4);
                *((long *) (SstackS)) = y_542X;
                SstackS = ((SstackS) + -4);
                arg0K0 = 2;
                goto L17320;}}
            else {
              if ((result_547X < 0)) {
                push_exception_continuationB(5, 1);
                *((long *) (SstackS)) = x_541X;
                SstackS = ((SstackS) + -4);
                *((long *) (SstackS)) = y_542X;
                SstackS = ((SstackS) + -4);
                arg0K0 = 2;
                goto L17320;}
              else {
                goto L26575;}}}
          else {
            push_exception_continuationB(5, 1);
            *((long *) (SstackS)) = x_541X;
            SstackS = ((SstackS) + -4);
            *((long *) (SstackS)) = y_542X;
            SstackS = ((SstackS) + -4);
            arg0K0 = 2;
            goto L17320;}}}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = x_541X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = y_542X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17320;}}
      break;
    case 86 : {
      x_549X = SvalS;
      if ((9 == (255 & x_549X))) {
        arg0K0 = 5;
        goto L39472;}
      else {
        arg0K0 = 1;
        goto L39472;}}
      break;
    case 87 : {
      SstackS = ((SstackS) + 4);
      arg2_550X = *((long *) (SstackS));
      if ((9 == (255 & arg2_550X))) {
        if ((9 == (255 & (SvalS)))) {
          x_551X = SvalS;
          if ((arg2_550X == x_551X)) {
            arg0K0 = 5;
            goto L28977;}
          else {
            arg0K0 = 1;
            goto L28977;}}
        else {
          goto L28950;}}
      else {
        goto L28950;}}
      break;
    case 88 : {
      SstackS = ((SstackS) + 4);
      arg2_552X = *((long *) (SstackS));
      if ((9 == (255 & arg2_552X))) {
        if ((9 == (255 & (SvalS)))) {
          x_553X = SvalS;
          if ((arg2_552X < x_553X)) {
            arg0K0 = 5;
            goto L28887;}
          else {
            arg0K0 = 1;
            goto L28887;}}
        else {
          goto L28860;}}
      else {
        goto L28860;}}
      break;
    case 89 : {
      if ((9 == (255 & (SvalS)))) {
        SvalS = (((((((((SvalS))>>8)))))<<2));
        Scode_pointerS = ((Scode_pointerS) + 1);
        arg1K0 = (Scode_pointerS);
        goto L19093;}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = (SvalS);
        SstackS = ((SstackS) + -4);
        arg0K0 = 1;
        goto L17320;}}
      break;
    case 90 : {
      if ((0 == (3 & (SvalS)))) {
        x_554X = (((SvalS))>>2);
        if ((255 < x_554X)) {
          goto L33722;}
        else {
          if ((x_554X < 0)) {
            goto L33722;}
          else {
            SvalS = (9 + (((((x_554X)))<<8)));
            Scode_pointerS = ((Scode_pointerS) + 1);
            arg1K0 = (Scode_pointerS);
            goto L19093;}}}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = (SvalS);
        SstackS = ((SstackS) + -4);
        arg0K0 = 1;
        goto L17320;}}
      break;
    case 91 : {
      x_555X = SvalS;
      if ((21 == x_555X)) {
        arg0K0 = 5;
        goto L39501;}
      else {
        arg0K0 = 1;
        goto L39501;}}
      break;
    case 92 : {
      x_556X = SvalS;
      type_557X = *((unsigned char *) ((Scode_pointerS) + 1));
      if ((3 == (3 & x_556X))) {
        if (((31 & ((((*((long *) ((((char *) (-3 + x_556X))) + -4))))>>2))) == type_557X)) {
          arg0K0 = 5;
          goto L39515;}
        else {
          arg0K0 = 1;
          goto L39515;}}
      else {
        arg0K0 = 1;
        goto L39515;}}
      break;
    case 93 : {
      stob_558X = SvalS;
      type_559X = *((unsigned char *) ((Scode_pointerS) + 1));
      if ((3 == (3 & stob_558X))) {
        if (((31 & ((((*((long *) ((((char *) (-3 + stob_558X))) + -4))))>>2))) == type_559X)) {
          SvalS = (-4 & (3 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + stob_558X))) + -4))))>>8))));
          Scode_pointerS = ((Scode_pointerS) + 2);
          arg1K0 = (Scode_pointerS);
          goto L19093;}
        else {
          goto L21893;}}
      else {
        goto L21893;}}
      break;
    case 94 : {
      len_560X = *((unsigned char *) ((Scode_pointerS) + 1));
      space_561X = 4 + (((len_560X)<<2));
      v_562X = AVAILABLEp(space_561X);
      if (v_562X) {
        goto L21965;}
      else {
        collect_saving_temps(1, 1, &temp1_563X);
        v_564X = AVAILABLEp(space_561X);
        if (v_564X) {
          goto L21965;}
        else {
          ps_error("Scheme48 heap overflow", 0);
          goto L21965;}}}
      break;
    case 95 : {
      SstackS = ((SstackS) + 4);
      len_565X = (((*((long *) (SstackS))))>>2);
      space_566X = 4 + (((len_565X)<<2));
      v_567X = AVAILABLEp(space_566X);
      if (v_567X) {
        goto L22147;}
      else {
        collect_saving_temps(1, 1, &temp1_568X);
        v_569X = AVAILABLEp(space_566X);
        if (v_569X) {
          goto L22147;}
        else {
          ps_error("Scheme48 heap overflow", 0);
          goto L22147;}}}
      break;
    case 96 : {
      stob_570X = SvalS;
      offset_571X = *((unsigned char *) ((Scode_pointerS) + 2));
      type_572X = *((unsigned char *) ((Scode_pointerS) + 1));
      if ((3 == (3 & stob_570X))) {
        if (((31 & ((((*((long *) ((((char *) (-3 + stob_570X))) + -4))))>>2))) == type_572X)) {
          SvalS = (*((long *) ((((char *) (-3 + stob_570X))) + (((offset_571X)<<2)))));
          Scode_pointerS = ((Scode_pointerS) + 3);
          arg1K0 = (Scode_pointerS);
          goto L19093;}
        else {
          goto L22357;}}
      else {
        goto L22357;}}
      break;
    case 97 : {
      SstackS = ((SstackS) + 4);
      stob_573X = *((long *) (SstackS));
      value_574X = SvalS;
      offset_575X = *((unsigned char *) ((Scode_pointerS) + 2));
      type_576X = *((unsigned char *) ((Scode_pointerS) + 1));
      if ((3 == (3 & stob_573X))) {
        if (((31 & ((((*((long *) ((((char *) (-3 + stob_573X))) + -4))))>>2))) == type_576X)) {
          if ((3 == (3 & stob_573X))) {
            if ((0 == (128 & (*((long *) ((((char *) (-3 + stob_573X))) + -4)))))) {
              addr_577X = (((char *) (-3 + stob_573X))) + (((offset_575X)<<2));
              S48_WRITE_BARRIER(stob_573X, addr_577X, value_574X);
              *((long *) addr_577X) = value_574X;
              SvalS = 13;
              Scode_pointerS = ((Scode_pointerS) + 3);
              arg1K0 = (Scode_pointerS);
              goto L19093;}
            else {
              goto L22460;}}
          else {
            goto L22460;}}
        else {
          goto L22460;}}
      else {
        goto L22460;}}
      break;
    case 98 : {
      SstackS = ((SstackS) + 4);
      len_578X = *((long *) (SstackS));
      init_579X = SvalS;
      type_580X = *((unsigned char *) ((Scode_pointerS) + 1));
      if ((0 == (3 & len_578X))) {
        len_581X = ((len_578X)>>2);
        size_582X = 1 + len_581X;
        if ((len_581X < 0)) {
          goto L22645;}
        else {
          if ((4194305 < size_582X)) {
            goto L22645;}
          else {
            v_583X = AVAILABLEp(size_582X);
            if (v_583X) {
              arg2K0 = 1;
              arg0K1 = init_579X;
              goto L22663;}
            else {
              temp0_584X = collect_saving_temps(init_579X, 1, &temp1_585X);
              v_586X = AVAILABLEp(size_582X);
              if (v_586X) {
                arg2K0 = 1;
                arg0K1 = temp0_584X;
                goto L22663;}
              else {
                arg2K0 = 0;
                arg0K1 = temp0_584X;
                goto L22663;}}}}}
      else {
        push_exception_continuationB(5, 2);
        *((long *) (SstackS)) = (((type_580X)<<2));
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = len_578X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = init_579X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 3;
        goto L17320;}}
      break;
    case 99 : {
      SstackS = ((SstackS) + 4);
      stob_587X = *((long *) (SstackS));
      index_588X = SvalS;
      type_589X = *((unsigned char *) ((Scode_pointerS) + 1));
      if ((0 == (3 & index_588X))) {
        if ((3 == (3 & stob_587X))) {
          if (((31 & ((((*((long *) ((((char *) (-3 + stob_587X))) + -4))))>>2))) == type_589X)) {
            len_590X = (((3 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + stob_587X))) + -4))))>>8))))>>2);
            index_591X = ((index_588X)>>2);
            if ((index_591X < 0)) {
              goto L22941;}
            else {
              if ((index_591X < len_590X)) {
                SvalS = (*((long *) ((((char *) (-3 + stob_587X))) + (-4 & index_588X))));
                Scode_pointerS = ((Scode_pointerS) + 2);
                arg1K0 = (Scode_pointerS);
                goto L19093;}
              else {
                goto L22941;}}}
          else {
            goto L22918;}}
        else {
          goto L22918;}}
      else {
        goto L22918;}}
      break;
    case 100 : {
      SstackS = ((SstackS) + 4);
      arg2_592X = *((long *) (SstackS));
      SstackS = ((SstackS) + 4);
      stob_593X = *((long *) (SstackS));
      value_594X = SvalS;
      type_595X = *((unsigned char *) ((Scode_pointerS) + 1));
      if ((0 == (3 & arg2_592X))) {
        if ((3 == (3 & stob_593X))) {
          if (((31 & ((((*((long *) ((((char *) (-3 + stob_593X))) + -4))))>>2))) == type_595X)) {
            if ((3 == (3 & stob_593X))) {
              if ((0 == (128 & (*((long *) ((((char *) (-3 + stob_593X))) + -4)))))) {
                len_596X = (((3 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + stob_593X))) + -4))))>>8))))>>2);
                index_597X = ((arg2_592X)>>2);
                if ((index_597X < 0)) {
                  goto L23181;}
                else {
                  if ((index_597X < len_596X)) {
                    addr_598X = (((char *) (-3 + stob_593X))) + (-4 & arg2_592X);
                    S48_WRITE_BARRIER(stob_593X, addr_598X, value_594X);
                    *((long *) addr_598X) = value_594X;
                    SvalS = 13;
                    Scode_pointerS = ((Scode_pointerS) + 2);
                    arg1K0 = (Scode_pointerS);
                    goto L19093;}
                  else {
                    goto L23181;}}}
              else {
                goto L23156;}}
            else {
              goto L23156;}}
          else {
            goto L23156;}}
        else {
          goto L23156;}}
      else {
        goto L23156;}}
      break;
    case 101 : {
      SstackS = ((SstackS) + 4);
      arg2_599X = *((long *) (SstackS));
      if ((0 == (3 & (arg2_599X | (SvalS))))) {
        len_600X = ((arg2_599X)>>2);
        init_601X = (((SvalS))>>2);
        size_602X = 1 + ((((3 + len_600X))>>2));
        if ((len_600X < 0)) {
          goto L26746;}
        else {
          if ((4194305 < size_602X)) {
            goto L26746;}
          else {
            v_603X = AVAILABLEp(size_602X);
            if (v_603X) {
              goto L26838;}
            else {
              collect_saving_temps(1, 1, &temp1_604X);
              v_605X = AVAILABLEp(size_602X);
              if (v_605X) {
                goto L26838;}
              else {
                push_exception_continuationB(8, 1);
                *((long *) (SstackS)) = (((len_600X)<<2));
                SstackS = ((SstackS) + -4);
                *((long *) (SstackS)) = (((init_601X)<<2));
                SstackS = ((SstackS) + -4);
                arg0K0 = 2;
                goto L17320;}}}}}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = arg2_599X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = (SvalS);
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17320;}}
      break;
    case 102 : {
      obj_606X = SvalS;
      if ((3 == (3 & obj_606X))) {
        if ((18 == (31 & ((((*((long *) ((((char *) (-3 + obj_606X))) + -4))))>>2))))) {
          SvalS = (((((long)(((unsigned long)(*((long *) ((((char *) (-3 + (SvalS)))) + -4))))>>8)))<<2));
          Scode_pointerS = ((Scode_pointerS) + 1);
          arg1K0 = (Scode_pointerS);
          goto L19093;}
        else {
          goto L37292;}}
      else {
        goto L37292;}}
      break;
    case 103 : {
      SstackS = ((SstackS) + 4);
      arg2_607X = *((long *) (SstackS));
      if ((3 == (3 & arg2_607X))) {
        if ((18 == (31 & ((((*((long *) ((((char *) (-3 + arg2_607X))) + -4))))>>2))))) {
          if ((0 == (3 & (SvalS)))) {
            index_608X = (((SvalS))>>2);
            len_609X = (long)(((unsigned long)(*((long *) ((((char *) (-3 + arg2_607X))) + -4))))>>8);
            if ((index_608X < 0)) {
              goto L33828;}
            else {
              if ((index_608X < len_609X)) {
                SvalS = ((((*((unsigned char *) ((((char *) (-3 + arg2_607X))) + index_608X))))<<2));
                Scode_pointerS = ((Scode_pointerS) + 1);
                arg1K0 = (Scode_pointerS);
                goto L19093;}
              else {
                goto L33828;}}}
          else {
            goto L36868;}}
        else {
          goto L36868;}}
      else {
        goto L36868;}}
      break;
    case 104 : {
      SstackS = ((SstackS) + 4);
      arg2_610X = *((long *) (SstackS));
      SstackS = ((SstackS) + 4);
      arg3_611X = *((long *) (SstackS));
      if ((3 == (3 & arg3_611X))) {
        if ((18 == (31 & ((((*((long *) ((((char *) (-3 + arg3_611X))) + -4))))>>2))))) {
          if ((0 == (3 & (arg2_610X | (SvalS))))) {
            index_612X = ((arg2_610X)>>2);
            Kchar_613X = (((SvalS))>>2);
            if ((3 == (3 & arg3_611X))) {
              if ((0 == (128 & (*((long *) ((((char *) (-3 + arg3_611X))) + -4)))))) {
                len_614X = (long)(((unsigned long)(*((long *) ((((char *) (-3 + arg3_611X))) + -4))))>>8);
                if ((index_612X < 0)) {
                  goto L30738;}
                else {
                  if ((index_612X < len_614X)) {
                    *((unsigned char *) ((((char *) (-3 + arg3_611X))) + index_612X)) = Kchar_613X;
                    SvalS = 13;
                    Scode_pointerS = ((Scode_pointerS) + 1);
                    arg1K0 = (Scode_pointerS);
                    goto L19093;}
                  else {
                    goto L30738;}}}
              else {
                goto L30717;}}
            else {
              goto L30717;}}
          else {
            goto L35280;}}
        else {
          goto L35280;}}
      else {
        goto L35280;}}
      break;
    case 105 : {
      SstackS = ((SstackS) + 4);
      arg2_615X = *((long *) (SstackS));
      if ((0 == (3 & arg2_615X))) {
        if ((9 == (255 & (SvalS)))) {
          len_616X = ((arg2_615X)>>2);
          init_617X = ((((SvalS))>>8));
          size_618X = 1 + ((((4 + len_616X))>>2));
          if ((len_616X < 0)) {
            goto L26980;}
          else {
            if ((4194305 < size_618X)) {
              goto L26980;}
            else {
              v_619X = AVAILABLEp(size_618X);
              if (v_619X) {
                goto L27072;}
              else {
                collect_saving_temps(1, 1, &temp1_620X);
                v_621X = AVAILABLEp(size_618X);
                if (v_621X) {
                  goto L27072;}
                else {
                  push_exception_continuationB(8, 1);
                  *((long *) (SstackS)) = (((len_616X)<<2));
                  SstackS = ((SstackS) + -4);
                  *((long *) (SstackS)) = (9 + ((((init_617X))<<8)));
                  SstackS = ((SstackS) + -4);
                  arg0K0 = 2;
                  goto L17320;}}}}}
        else {
          goto L33909;}}
      else {
        goto L33909;}}
      break;
    case 106 : {
      obj_622X = SvalS;
      if ((3 == (3 & obj_622X))) {
        if ((17 == (31 & ((((*((long *) ((((char *) (-3 + obj_622X))) + -4))))>>2))))) {
          SvalS = (-4 + (((((long)(((unsigned long)(*((long *) ((((char *) (-3 + (SvalS)))) + -4))))>>8)))<<2)));
          Scode_pointerS = ((Scode_pointerS) + 1);
          arg1K0 = (Scode_pointerS);
          goto L19093;}
        else {
          goto L37342;}}
      else {
        goto L37342;}}
      break;
    case 107 : {
      SstackS = ((SstackS) + 4);
      arg2_623X = *((long *) (SstackS));
      if ((3 == (3 & arg2_623X))) {
        if ((17 == (31 & ((((*((long *) ((((char *) (-3 + arg2_623X))) + -4))))>>2))))) {
          if ((0 == (3 & (SvalS)))) {
            index_624X = (((SvalS))>>2);
            len_625X = -1 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + arg2_623X))) + -4))))>>8));
            if ((index_624X < 0)) {
              goto L33972;}
            else {
              if ((index_624X < len_625X)) {
                SvalS = (9 + ((((((*((unsigned char *) ((((char *) (-3 + arg2_623X))) + index_624X))))))<<8)));
                Scode_pointerS = ((Scode_pointerS) + 1);
                arg1K0 = (Scode_pointerS);
                goto L19093;}
              else {
                goto L33972;}}}
          else {
            goto L36964;}}
        else {
          goto L36964;}}
      else {
        goto L36964;}}
      break;
    case 108 : {
      SstackS = ((SstackS) + 4);
      arg2_626X = *((long *) (SstackS));
      SstackS = ((SstackS) + 4);
      arg3_627X = *((long *) (SstackS));
      if ((3 == (3 & arg3_627X))) {
        if ((17 == (31 & ((((*((long *) ((((char *) (-3 + arg3_627X))) + -4))))>>2))))) {
          if ((0 == (3 & arg2_626X))) {
            if ((9 == (255 & (SvalS)))) {
              index_628X = ((arg2_626X)>>2);
              Kchar_629X = ((((SvalS))>>8));
              if ((3 == (3 & arg3_627X))) {
                if ((0 == (128 & (*((long *) ((((char *) (-3 + arg3_627X))) + -4)))))) {
                  len_630X = -1 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + arg3_627X))) + -4))))>>8));
                  if ((index_628X < 0)) {
                    goto L30893;}
                  else {
                    if ((index_628X < len_630X)) {
                      *((unsigned char *) ((((char *) (-3 + arg3_627X))) + index_628X)) = (Kchar_629X);
                      SvalS = 13;
                      Scode_pointerS = ((Scode_pointerS) + 1);
                      arg1K0 = (Scode_pointerS);
                      goto L19093;}
                    else {
                      goto L30893;}}}
                else {
                  goto L30872;}}
              else {
                goto L30872;}}
            else {
              goto L35406;}}
          else {
            goto L35406;}}
        else {
          goto L35406;}}
      else {
        goto L35406;}}
      break;
    case 109 : {
      v_631X = AVAILABLEp(3);
      if (v_631X) {
        goto L25150;}
      else {
        collect_saving_temps(1, 1, &temp1_632X);
        v_633X = AVAILABLEp(3);
        if (v_633X) {
          goto L25150;}
        else {
          ps_error("Scheme48 heap overflow", 0);
          goto L25150;}}}
      break;
    case 110 : {
      obj_634X = SvalS;
      if ((3 == (3 & obj_634X))) {
        if ((4 == (31 & ((((*((long *) ((((char *) (-3 + obj_634X))) + -4))))>>2))))) {
          x_635X = SvalS;
          descriptor_636X = *((long *) ((((char *) (-3 + x_635X))) + 4));
          if ((17 == (255 & descriptor_636X))) {
            if ((529 == (*((long *) ((((char *) (-3 + x_635X))) + 4))))) {
              arg0K0 = 5;
              goto L34953;}
            else {
              arg0K0 = 1;
              goto L34953;}}
          else {
            arg0K0 = 5;
            goto L34953;}}
        else {
          goto L34934;}}
      else {
        goto L34934;}}
      break;
    case 111 : {
      SstackS = ((SstackS) + 4);
      arg2_637X = *((long *) (SstackS));
      if ((3 == (3 & arg2_637X))) {
        if ((4 == (31 & ((((*((long *) ((((char *) (-3 + arg2_637X))) + -4))))>>2))))) {
          x_638X = SvalS;
          if ((1 == x_638X)) {
            goto L30480;}
          else {
            if ((5 == x_638X)) {
              goto L30480;}
            else {
              goto L30487;}}}
        else {
          goto L30487;}}
      else {
        goto L30487;}}
      break;
    case 112 : {
      x_639X = SvalS;
      if ((3 == (3 & x_639X))) {
        if ((0 == (128 & (*((long *) ((((char *) (-3 + x_639X))) + -4)))))) {
          arg0K0 = 1;
          goto L39587;}
        else {
          arg0K0 = 5;
          goto L39587;}}
      else {
        arg0K0 = 5;
        goto L39587;}}
      break;
    case 113 : {
      x_640X = SvalS;
      if ((3 == (3 & x_640X))) {
        if ((0 == (128 & (*((long *) ((((char *) (-3 + x_640X))) + -4)))))) {
          *((long *) ((((char *) (-3 + x_640X))) + -4)) = (128 | (*((long *) ((((char *) (-3 + x_640X))) + -4))));
          goto L34054;}
        else {
          goto L34054;}}
      else {
        goto L34054;}}
      break;
    case 114 : {
      v_641X = AVAILABLEp(72);
      if (v_641X) {
        arg2K0 = 1;
        arg0K1 = 0;
        goto L34105;}
      else {
        collect_saving_temps(1, 1, &temp1_642X);
        v_643X = AVAILABLEp(72);
        if (v_643X) {
          arg2K0 = 1;
          arg0K1 = 0;
          goto L34105;}
        else {
          arg2K0 = 0;
          arg0K1 = 0;
          goto L34105;}}}
      break;
    case 115 : {
      v_644X = AVAILABLEp(66);
      if (v_644X) {
        arg2K0 = 1;
        arg0K1 = 0;
        goto L35524;}
      else {
        collect_saving_temps(1, 1, &temp1_645X);
        v_646X = AVAILABLEp(66);
        if (v_646X) {
          arg2K0 = 1;
          arg0K1 = 0;
          goto L35524;}
        else {
          arg2K0 = 0;
          arg0K1 = 0;
          goto L35524;}}}
      break;
    case 116 : {
      v_647X = AVAILABLEp(66);
      if (v_647X) {
        arg2K0 = 1;
        arg0K1 = 0;
        goto L38430;}
      else {
        collect_saving_temps(1, 1, &temp1_648X);
        v_649X = AVAILABLEp(66);
        if (v_649X) {
          arg2K0 = 1;
          arg0K1 = 0;
          goto L38430;}
        else {
          arg2K0 = 0;
          arg0K1 = 0;
          goto L38430;}}}
      break;
    case 117 : {
      v_650X = AVAILABLEp(66);
      if (v_650X) {
        arg2K0 = 1;
        arg0K1 = 0;
        goto L38690;}
      else {
        collect_saving_temps(1, 1, &temp1_651X);
        v_652X = AVAILABLEp(66);
        if (v_652X) {
          arg2K0 = 1;
          arg0K1 = 0;
          goto L38690;}
        else {
          arg2K0 = 0;
          arg0K1 = 0;
          goto L38690;}}}
      break;
    case 118 : {
      SstackS = ((SstackS) + 4);
      arg2_653X = *((long *) (SstackS));
      if ((3 == (3 & arg2_653X))) {
        if ((6 == (31 & ((((*((long *) ((((char *) (-3 + arg2_653X))) + -4))))>>2))))) {
          x_654X = SvalS;
          if ((1 == x_654X)) {
            goto L28629;}
          else {
            if ((5 == x_654X)) {
              goto L28629;}
            else {
              goto L28636;}}}
        else {
          goto L28636;}}
      else {
        goto L28636;}}
      break;
    case 119 : {
      v_655X = AVAILABLEp(66);
      if (v_655X) {
        arg2K0 = 1;
        arg0K1 = 0;
        goto L35009;}
      else {
        collect_saving_temps(1, 1, &temp1_656X);
        v_657X = AVAILABLEp(66);
        if (v_657X) {
          arg2K0 = 1;
          arg0K1 = 0;
          goto L35009;}
        else {
          arg2K0 = 0;
          arg0K1 = 0;
          goto L35009;}}}
      break;
    case 120 : {
      obj_658X = SvalS;
      if ((3 == (3 & obj_658X))) {
        if ((6 == (31 & ((((*((long *) ((((char *) (-3 + obj_658X))) + -4))))>>2))))) {
          channel_659X = SvalS;
          temp_660X = 4 == (*((long *) (((char *) (-3 + channel_659X)))));
          if (temp_660X) {
            arg2K0 = temp_660X;
            goto L8509;}
          else {
            arg2K0 = (12 == (*((long *) (((char *) (-3 + channel_659X))))));
            goto L8509;}}
        else {
          goto L34159;}}
      else {
        goto L34159;}}
      break;
    case 121 : {
      space_661X = 3 * (Snumber_of_channelsS);
      v_662X = AVAILABLEp(space_661X);
      if (v_662X) {
        goto L15425;}
      else {
        collect_saving_temps(1, 1, &temp1_663X);
        v_664X = AVAILABLEp(space_661X);
        if (v_664X) {
          goto L15425;}
        else {
          ps_error("Scheme48 heap overflow", 0);
          goto L15425;}}}
      break;
    case 122 : {
      SvalS = 529;
      Scode_pointerS = ((Scode_pointerS) + 1);
      arg1K0 = (Scode_pointerS);
      goto L19093;}
      break;
    case 123 : {
      SvalS = 13;
      Scode_pointerS = ((Scode_pointerS) + 1);
      arg1K0 = (Scode_pointerS);
      goto L19093;}
      break;
    case 124 : {
      x_665X = SvalS;
      push_exception_continuationB(16, 1);
      *((long *) (SstackS)) = x_665X;
      SstackS = ((SstackS) + -4);
      arg0K0 = 1;
      goto L17320;}
      break;
    case 125 : {
      SvalS = 1;
      Scode_pointerS = ((Scode_pointerS) + 1);
      arg1K0 = (Scode_pointerS);
      goto L19093;}
      break;
    case 126 : {
      SvalS = 21;
      Scode_pointerS = ((Scode_pointerS) + 1);
      arg1K0 = (Scode_pointerS);
      goto L19093;}
      break;
    case 127 : {
      v_666X = AVAILABLEp(66);
      if (v_666X) {
        arg2K0 = 1;
        arg0K1 = 0;
        goto L35625;}
      else {
        collect_saving_temps(1, 1, &temp1_667X);
        v_668X = AVAILABLEp(66);
        if (v_668X) {
          arg2K0 = 1;
          arg0K1 = 0;
          goto L35625;}
        else {
          arg2K0 = 0;
          arg0K1 = 0;
          goto L35625;}}}
      break;
    case 128 : {
      SvalS = 13;
      collect_saving_temps(0, 0, &v_669X);
      Scode_pointerS = ((Scode_pointerS) + 1);
      arg1K0 = (Scode_pointerS);
      goto L19093;}
      break;
    case 129 : {
      obj_670X = SvalS;
      if ((3 == (3 & obj_670X))) {
        if ((17 == (31 & ((((*((long *) ((((char *) (-3 + obj_670X))) + -4))))>>2))))) {
          x_671X = SvalS;
          n_672X = -1 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + x_671X))) + -4))))>>8));
          arg0K0 = 0;
          arg0K1 = 0;
          goto L36457;}
        else {
          goto L36434;}}
      else {
        goto L36434;}}
      break;
    case 130 : {
      v_673X = AVAILABLEp(6);
      if (v_673X) {
        goto L39657;}
      else {
        collect_saving_temps(1, 1, &temp1_674X);
        v_675X = AVAILABLEp(6);
        if (v_675X) {
          goto L39657;}
        else {
          ps_error("Scheme48 heap overflow", 0);
          goto L39657;}}}
      break;
    case 131 : {
      SstackS = ((SstackS) + 4);
      arg2_676X = *((long *) (SstackS));
      if ((0 == (3 & arg2_676X))) {
        key_677X = ((arg2_676X)>>2);
        other_678X = SvalS;
        if ((5 == key_677X)) {
          SvalS = (-4 & other_678X);
          Scode_pointerS = ((Scode_pointerS) + 1);
          arg1K0 = (Scode_pointerS);
          goto L19093;}
        else {
          if ((0 == key_677X)) {
            x_679X = s48_available();
            SvalS = (((x_679X)<<2));
            Scode_pointerS = ((Scode_pointerS) + 1);
            arg1K0 = (Scode_pointerS);
            goto L19093;}
          else {
            if ((1 == key_677X)) {
              bytes_680X = s48_heap_size();
              SvalS = (-4 & (3 + bytes_680X));
              Scode_pointerS = ((Scode_pointerS) + 1);
              arg1K0 = (Scode_pointerS);
              goto L19093;}
            else {
              if ((2 == key_677X)) {
                SvalS = (((((Sstack_endS) - (Sstack_beginS)))<<2));
                Scode_pointerS = ((Scode_pointerS) + 1);
                arg1K0 = (Scode_pointerS);
                goto L19093;}
              else {
                if ((3 == key_677X)) {
                  x_681X = s48_gc_count();
                  SvalS = (((x_681X)<<2));
                  Scode_pointerS = ((Scode_pointerS) + 1);
                  arg1K0 = (Scode_pointerS);
                  goto L19093;}
                else {
                  if ((4 == key_677X)) {
                    push_exception_continuationB(15, 1);
                    *((long *) (SstackS)) = (((key_677X)<<2));
                    SstackS = ((SstackS) + -4);
                    *((long *) (SstackS)) = other_678X;
                    SstackS = ((SstackS) + -4);
                    arg0K0 = 2;
                    goto L17320;}
                  else {
                    push_exception_continuationB(18, 1);
                    *((long *) (SstackS)) = (((key_677X)<<2));
                    SstackS = ((SstackS) + -4);
                    *((long *) (SstackS)) = other_678X;
                    SstackS = ((SstackS) + -4);
                    arg0K0 = 2;
                    goto L17320;}}}}}}}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = arg2_676X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = (SvalS);
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17320;}}
      break;
    case 132 : {
      if ((0 == (3 & (SvalS)))) {
        type_682X = (((SvalS))>>2);
        arg2K0 = 1;
        goto L34267;}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = (SvalS);
        SstackS = ((SstackS) + -4);
        arg0K0 = 1;
        goto L17320;}}
      break;
    case 133 : {
      x_683X = SvalS;
      arg2K0 = 1;
      arg0K1 = x_683X;
      goto L39707;}
      break;
    case 134 : {
      SvalS = (Scurrent_threadS);
      Scode_pointerS = ((Scode_pointerS) + 1);
      arg1K0 = (Scode_pointerS);
      goto L19093;}
      break;
    case 135 : {
      Scurrent_threadS = (SvalS);
      SvalS = 13;
      Scode_pointerS = ((Scode_pointerS) + 1);
      arg1K0 = (Scode_pointerS);
      goto L19093;}
      break;
    case 136 : {
      SvalS = (Ssession_dataS);
      Scode_pointerS = ((Scode_pointerS) + 1);
      arg1K0 = (Scode_pointerS);
      goto L19093;}
      break;
    case 137 : {
      Ssession_dataS = (SvalS);
      SvalS = 13;
      Scode_pointerS = ((Scode_pointerS) + 1);
      arg1K0 = (Scode_pointerS);
      goto L19093;}
      break;
    case 138 : {
      obj_684X = SvalS;
      if ((3 == (3 & obj_684X))) {
        if ((2 == (31 & ((((*((long *) ((((char *) (-3 + obj_684X))) + -4))))>>2))))) {
          if ((((((3 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + (SvalS)))) + -4))))>>8))))>>2)) < 162)) {
            goto L21234;}
          else {
            temp_685X = Sexception_handlersS;
            Sexception_handlersS = (SvalS);
            SvalS = temp_685X;
            Scode_pointerS = ((Scode_pointerS) + 1);
            arg1K0 = (Scode_pointerS);
            goto L19093;}}
        else {
          goto L21234;}}
      else {
        goto L21234;}}
      break;
    case 139 : {
      SstackS = ((SstackS) + 4);
      p_686X = *((long *) (SstackS));
      SstackS = ((SstackS) + 4);
      template_687X = *((long *) (SstackS));
      SstackS = ((SstackS) + 4);
      SstackS = ((SstackS) + 4);
      p_688X = *((long *) (SstackS));
      StemplateS = template_687X;
      Scode_pointerS = ((((char *) (-3 + (*((long *) (((char *) (-3 + template_687X)))))))) + ((((((((((p_686X)>>2)) + (((p_688X)>>2))))<<2)))>>2)));
      arg1K0 = (Scode_pointerS);
      goto L19093;}
      break;
    case 140 : {
      obj_689X = SvalS;
      if ((3 == (3 & obj_689X))) {
        if ((2 == (31 & ((((*((long *) ((((char *) (-3 + obj_689X))) + -4))))>>2))))) {
          if ((((((3 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + (SvalS)))) + -4))))>>8))))>>2)) < 6)) {
            goto L20149;}
          else {
            temp_690X = Sinterrupt_handlersS;
            Sinterrupt_handlersS = (SvalS);
            SvalS = temp_690X;
            Scode_pointerS = ((Scode_pointerS) + 1);
            arg1K0 = (Scode_pointerS);
            goto L19093;}}
        else {
          goto L20149;}}
      else {
        goto L20149;}}
      break;
    case 141 : {
      old_691X = Senabled_interruptsS;
      p_692X = SvalS;
      Senabled_interruptsS = (((p_692X)>>2));
      if ((0 == ((Spending_interruptsS) & (Senabled_interruptsS)))) {
        s48_Spending_interruptPS = 0;
        if ((s48_Spending_eventsPS)) {
          s48_Spending_interruptPS = 1;
          goto L39748;}
        else {
          goto L39748;}}
      else {
        s48_Spending_interruptPS = 1;
        goto L39748;}}
      break;
    case 142 : {
      SstackS = ((SstackS) + 4);
      p_693X = *((long *) (SstackS));
      Senabled_interruptsS = (((p_693X)>>2));
      if ((0 == ((Spending_interruptsS) & (Senabled_interruptsS)))) {
        s48_Spending_interruptPS = 0;
        if ((s48_Spending_eventsPS)) {
          s48_Spending_interruptPS = 1;
          goto L19107;}
        else {
          goto L19107;}}
      else {
        s48_Spending_interruptPS = 1;
        goto L19107;}}
      break;
    case 143 : {
      if ((0 == (3 & (SvalS)))) {
        p_694X = SvalS;
        Spending_interruptsS = (-2 & (Spending_interruptsS));
        if ((0 == ((Spending_interruptsS) & (Senabled_interruptsS)))) {
          s48_Spending_interruptPS = 0;
          if ((s48_Spending_eventsPS)) {
            s48_Spending_interruptPS = 1;
            goto L38250;}
          else {
            goto L38250;}}
        else {
          s48_Spending_interruptPS = 1;
          goto L38250;}}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = (SvalS);
        SstackS = ((SstackS) + -4);
        arg0K0 = 1;
        goto L17320;}}
      break;
    case 144 : {
      SstackS = ((SstackS) + 4);
      arg2_695X = *((long *) (SstackS));
      if ((0 == (3 & arg2_695X))) {
        x_696X = SvalS;
        if ((1 == x_696X)) {
          goto L35153;}
        else {
          if ((5 == x_696X)) {
            goto L35153;}
          else {
            goto L35158;}}}
      else {
        goto L35158;}}
      break;
    case 145 : {
      SstackS = ((SstackS) + 4);
      nargs_697X = (((*((long *) (SstackS))))>>2);
      SstackS = ((SstackS) + 4);
      p_698X = *((long *) (SstackS));
      SstackS = ((SstackS) + 4);
      rest_list_699X = *((long *) (SstackS));
      if ((14 < nargs_697X)) {
        push_exception_continuationB(20, 1);
        *((long *) (SstackS)) = (*((long *) ((SstackS) + (-4 & p_698X))));
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = nargs_697X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17320;}
      else {
        arg0K0 = rest_list_699X;
        goto L23453;}}
      break;
    case 146 : {
      v_700X = AVAILABLEp(5);
      if (v_700X) {
        arg2K0 = 1;
        arg0K1 = 0;
        goto L34376;}
      else {
        collect_saving_temps(1, 1, &temp1_701X);
        v_702X = AVAILABLEp(5);
        if (v_702X) {
          arg2K0 = 1;
          arg0K1 = 0;
          goto L34376;}
        else {
          arg2K0 = 0;
          arg0K1 = 0;
          goto L34376;}}}
      break;
    case 148 : {
      SstackS = ((SstackS) + 4);
      arg2_703X = *((long *) (SstackS));
      if ((3 == (3 & arg2_703X))) {
        if ((17 == (31 & ((((*((long *) ((((char *) (-3 + arg2_703X))) + -4))))>>2))))) {
          x_704X = SvalS;
          if ((1 == x_704X)) {
            goto L24577;}
          else {
            if ((5 == x_704X)) {
              goto L24577;}
            else {
              goto L24582;}}}
        else {
          goto L24582;}}
      else {
        goto L24582;}}
      break;
    case 149 : {
      SstackS = ((SstackS) + 4);
      arg2_705X = *((long *) (SstackS));
      if ((0 == (3 & arg2_705X))) {
        option_706X = ((arg2_705X)>>2);
        other_707X = SvalS;
        if ((2 == option_706X)) {
          x_708X = CHEAP_TIME();
          SvalS = (((x_708X)<<2));
          Scode_pointerS = ((Scode_pointerS) + 1);
          arg1K0 = (Scode_pointerS);
          goto L19093;}
        else {
          if ((0 == option_706X)) {
            seconds_709X = s48_run_time(&mseconds_710X);
            arg0K0 = option_706X;
            arg0K1 = seconds_709X;
            arg0K2 = mseconds_710X;
            goto L36368;}
          else {
            if ((1 == option_706X)) {
              seconds_711X = s48_real_time(&mseconds_712X);
              arg0K0 = option_706X;
              arg0K1 = seconds_711X;
              arg0K2 = mseconds_712X;
              goto L36368;}
            else {
              push_exception_continuationB(18, 1);
              *((long *) (SstackS)) = (((option_706X)<<2));
              SstackS = ((SstackS) + -4);
              *((long *) (SstackS)) = other_707X;
              SstackS = ((SstackS) + -4);
              arg0K0 = 2;
              goto L17320;}}}}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = arg2_705X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = (SvalS);
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17320;}}
      break;
    case 150 : {
      SstackS = ((SstackS) + 4);
      arg2_713X = *((long *) (SstackS));
      if ((0 == (3 & arg2_713X))) {
        key_714X = ((arg2_713X)>>2);
        value_715X = SvalS;
        status_716X = s48_extended_vm(key_714X, value_715X);
        if ((0 == status_716X)) {
          SvalS = (s48_Sextension_valueS);
          Scode_pointerS = ((Scode_pointerS) + 1);
          arg1K0 = (Scode_pointerS);
          goto L19093;}
        else {
          if ((1 == status_716X)) {
            push_exception_continuationB(23, 1);
            *((long *) (SstackS)) = (((key_714X)<<2));
            SstackS = ((SstackS) + -4);
            *((long *) (SstackS)) = value_715X;
            SstackS = ((SstackS) + -4);
            arg0K0 = 2;
            goto L17320;}
          else {
            push_exception_continuationB(24, 1);
            *((long *) (SstackS)) = (((key_714X)<<2));
            SstackS = ((SstackS) + -4);
            *((long *) (SstackS)) = value_715X;
            SstackS = ((SstackS) + -4);
            arg0K0 = 2;
            goto L17320;}}}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = arg2_713X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = (SvalS);
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17320;}}
      break;
    case 151 : {
      SstackS = ((SstackS) + 4);
      arg2_717X = *((long *) (SstackS));
      x_718X = SvalS;
      Senabled_interruptsS = -1;
      if ((0 == ((Spending_interruptsS) & (Senabled_interruptsS)))) {
        s48_Spending_interruptPS = 0;
        if ((s48_Spending_eventsPS)) {
          s48_Spending_interruptPS = 1;
          goto L39808;}
        else {
          goto L39808;}}
      else {
        s48_Spending_interruptPS = 1;
        goto L39808;}}
      break;
    case 152 : {
      SstackS = ((SstackS) + 4);
      arg2_719X = *((long *) (SstackS));
      if ((3 == (3 & arg2_719X))) {
        if ((17 == (31 & ((((*((long *) ((((char *) (-3 + arg2_719X))) + -4))))>>2))))) {
          obj_720X = SvalS;
          if ((3 == (3 & obj_720X))) {
            if ((17 == (31 & ((((*((long *) ((((char *) (-3 + obj_720X))) + -4))))>>2))))) {
              x_721X = SvalS;
              len_722X = (long)(((unsigned long)(*((long *) ((((char *) (-3 + arg2_719X))) + -4))))>>8);
              if ((len_722X == ((long)(((unsigned long)(*((long *) ((((char *) (-3 + x_721X))) + -4))))>>8)))) {
                if (((!memcmp((void *)(((char *) (-3 + x_721X))), (void *)(((char *) (-3 + arg2_719X))),len_722X)))) {
                  arg0K0 = 5;
                  goto L28527;}
                else {
                  arg0K0 = 1;
                  goto L28527;}}
              else {
                arg0K0 = 1;
                goto L28527;}}
            else {
              goto L28494;}}
          else {
            goto L28494;}}
        else {
          goto L28494;}}
      else {
        goto L28494;}}
      break;
    case 153 : {
      space_723X = 1 + ((((4 + ((((SvalS))>>2))))>>2));
      v_724X = AVAILABLEp(space_723X);
      if (v_724X) {
        goto L34585;}
      else {
        collect_saving_temps(1, 1, &temp1_725X);
        v_726X = AVAILABLEp(space_723X);
        if (v_726X) {
          goto L34585;}
        else {
          ps_error("Scheme48 heap overflow", 0);
          goto L34585;}}}
      break;
    case 154 : {
      SstackS = ((SstackS) + 4);
      thing_727X = *((long *) (SstackS));
      list_728X = SvalS;
      arg0K0 = list_728X;
      arg0K1 = list_728X;
      arg2K2 = 1;
      goto L30135;}
      break;
    case 155 : {
      SstackS = ((SstackS) + 4);
      arg2_729X = *((long *) (SstackS));
      SstackS = ((SstackS) + 4);
      arg3_730X = *((long *) (SstackS));
      if ((0 == (3 & (SvalS)))) {
        index_731X = (((SvalS))>>2);
        if ((3 == (3 & arg3_730X))) {
          if ((9 == (31 & ((((*((long *) ((((char *) (-3 + arg3_730X))) + -4))))>>2))))) {
            if ((arg2_729X == (*((long *) (((char *) (-3 + arg3_730X))))))) {
              len_732X = (((3 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + arg3_730X))) + -4))))>>8))))>>2);
              if ((index_731X < 0)) {
                goto L28200;}
              else {
                if ((index_731X < len_732X)) {
                  SvalS = (*((long *) ((((char *) (-3 + arg3_730X))) + (((index_731X)<<2)))));
                  Scode_pointerS = ((Scode_pointerS) + 1);
                  arg1K0 = (Scode_pointerS);
                  goto L19093;}
                else {
                  goto L28200;}}}
            else {
              goto L28220;}}
          else {
            goto L28220;}}
        else {
          goto L28220;}}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = arg3_730X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = arg2_729X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = (SvalS);
        SstackS = ((SstackS) + -4);
        arg0K0 = 3;
        goto L17320;}}
      break;
    case 156 : {
      SstackS = ((SstackS) + 4);
      arg2_733X = *((long *) (SstackS));
      SstackS = ((SstackS) + 4);
      arg3_734X = *((long *) (SstackS));
      SstackS = ((SstackS) + 4);
      arg4_735X = *((long *) (SstackS));
      if ((0 == (3 & arg2_733X))) {
        index_736X = ((arg2_733X)>>2);
        value_737X = SvalS;
        if ((3 == (3 & arg4_735X))) {
          if ((9 == (31 & ((((*((long *) ((((char *) (-3 + arg4_735X))) + -4))))>>2))))) {
            if ((arg3_734X == (*((long *) (((char *) (-3 + arg4_735X))))))) {
              if ((3 == (3 & arg4_735X))) {
                if ((0 == (128 & (*((long *) ((((char *) (-3 + arg4_735X))) + -4)))))) {
                  len_738X = (((3 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + arg4_735X))) + -4))))>>8))))>>2);
                  if ((index_736X < 0)) {
                    goto L31678;}
                  else {
                    if ((index_736X < len_738X)) {
                      addr_739X = (((char *) (-3 + arg4_735X))) + (((index_736X)<<2));
                      S48_WRITE_BARRIER(arg4_735X, addr_739X, value_737X);
                      *((long *) addr_739X) = value_737X;
                      SvalS = 13;
                      Scode_pointerS = ((Scode_pointerS) + 1);
                      arg1K0 = (Scode_pointerS);
                      goto L19093;}
                    else {
                      goto L31678;}}}
                else {
                  goto L31700;}}
              else {
                goto L31700;}}
            else {
              goto L31700;}}
          else {
            goto L31700;}}
        else {
          goto L31700;}}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = arg4_735X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = arg3_734X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = arg2_733X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = (SvalS);
        SstackS = ((SstackS) + -4);
        arg0K0 = 4;
        goto L17320;}}
      break;
    case 157 : {
      SstackS = ((SstackS) + 4);
      arg2_740X = *((long *) (SstackS));
      SstackS = ((SstackS) + 4);
      arg3_741X = *((long *) (SstackS));
      SstackS = ((SstackS) + 4);
      arg4_742X = *((long *) (SstackS));
      SstackS = ((SstackS) + 4);
      arg5_743X = *((long *) (SstackS));
      if ((0 == (3 & ((arg4_742X | arg2_740X) | (SvalS))))) {
        from_index_744X = ((arg4_742X)>>2);
        to_index_745X = ((arg2_740X)>>2);
        count_746X = (((SvalS))>>2);
        if ((3 == (3 & arg5_743X))) {
          if ((17 == (31 & ((((*((long *) ((((char *) (-3 + arg5_743X))) + -4))))>>2))))) {
            goto L31929;}
          else {
            goto L31924;}}
        else {
          goto L31924;}}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = arg5_743X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = arg4_742X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = arg3_741X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = arg2_740X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = (SvalS);
        SstackS = ((SstackS) + -4);
        arg0K0 = 5;
        goto L17320;}}
      break;
    case 158 : {
      v_747X = *((unsigned char *) ((Scode_pointerS) + 1));
      if ((0 == v_747X)) {
        arg0K0 = (SvalS);
        goto L23709;}
      else {
        merged_arg0K0 = 0;
        get_current_port_return_tag = 0;
        goto get_current_port;
       get_current_port_return_0:
        v_748X = get_current_port0_return_value;
        arg0K0 = v_748X;
        goto L23709;}}
      break;
    case 159 : {
      v_749X = *((unsigned char *) ((Scode_pointerS) + 1));
      if ((0 == v_749X)) {
        arg0K0 = (SvalS);
        goto L23929;}
      else {
        merged_arg0K0 = 0;
        get_current_port_return_tag = 1;
        goto get_current_port;
       get_current_port_return_1:
        v_750X = get_current_port0_return_value;
        arg0K0 = v_750X;
        goto L23929;}}
      break;
    case 160 : {
      v_751X = *((unsigned char *) ((Scode_pointerS) + 1));
      if ((0 == v_751X)) {
        v_752X = SvalS;
        SstackS = ((SstackS) + 4);
        arg0K0 = (*((long *) (SstackS)));
        arg0K1 = v_752X;
        goto L24142;}
      else {
        merged_arg0K0 = 4;
        get_current_port_return_tag = 2;
        goto get_current_port;
       get_current_port_return_2:
        v_753X = get_current_port0_return_value;
        arg0K0 = (SvalS);
        arg0K1 = v_753X;
        goto L24142;}}
      break;
    case 161 : {
      x_754X = SvalS;
      out_755X = stderr;
      arg0K0 = x_754X;
      goto L30388;}
      break;
  }}
 L17320: {
  nargs_756X = arg0K0;
  opcode_757X = (((*((long *) ((SstackS) + (8 + (((nargs_756X)<<2)))))))>>2);
  obj_758X = Sexception_handlersS;
  if ((3 == (3 & obj_758X))) {
    if ((2 == (31 & ((((*((long *) ((((char *) (-3 + obj_758X))) + -4))))>>2))))) {
      goto L17379;}
    else {
      goto L17452;}}
  else {
    goto L17452;}}
 L24496: {
  env_759X = arg0K0;
  i_760X = arg0K1;
  if ((0 == i_760X)) {
    SvalS = (*((long *) ((((char *) (-3 + env_759X))) + ((((*((unsigned char *) ((Scode_pointerS) + 2))))<<2)))));
    if ((529 == (SvalS))) {
      push_exception_continuationB(0, 3);
      arg0K0 = 0;
      goto L17320;}
    else {
      Scode_pointerS = ((Scode_pointerS) + 3);
      arg1K0 = (Scode_pointerS);
      goto L19093;}}
  else {
    arg0K0 = (*((long *) (((char *) (-3 + env_759X)))));
    arg0K1 = (-1 + i_760X);
    goto L24496;}}
 L20911: {
  env_761X = arg0K0;
  i_762X = arg0K1;
  if ((0 == i_762X)) {
    SvalS = (*((long *) ((((char *) (-3 + env_761X))) + ((((((((*((unsigned char *) ((Scode_pointerS) + 3))))<<8)) + (*((unsigned char *) ((Scode_pointerS) + 4)))))<<2)))));
    if ((529 == (SvalS))) {
      push_exception_continuationB(0, 5);
      arg0K0 = 0;
      goto L17320;}
    else {
      Scode_pointerS = ((Scode_pointerS) + 5);
      arg1K0 = (Scode_pointerS);
      goto L19093;}}
  else {
    arg0K0 = (*((long *) (((char *) (-3 + env_761X)))));
    arg0K1 = (-1 + i_762X);
    goto L20911;}}
 L20849: {
  env_763X = arg0K0;
  i_764X = arg0K1;
  if ((0 == i_764X)) {
    index_765X = ((((*((unsigned char *) ((Scode_pointerS) + 3))))<<8)) + (*((unsigned char *) ((Scode_pointerS) + 4)));
    value_766X = SvalS;
    addr_767X = (((char *) (-3 + env_763X))) + (((index_765X)<<2));
    S48_WRITE_BARRIER(env_763X, addr_767X, value_766X);
    *((long *) addr_767X) = value_766X;
    SvalS = 13;
    Scode_pointerS = ((Scode_pointerS) + 5);
    arg1K0 = (Scode_pointerS);
    goto L19093;}
  else {
    arg0K0 = (*((long *) (((char *) (-3 + env_763X)))));
    arg0K1 = (-1 + i_764X);
    goto L20849;}}
 L21628: {
  space_768X = arg0K0;
  v_769X = AVAILABLEp(space_768X);
  if (v_769X) {
    arg2K0 = 1;
    arg0K1 = 0;
    goto L21702;}
  else {
    collect_saving_temps(1, 1, &temp1_770X);
    v_771X = AVAILABLEp(space_768X);
    if (v_771X) {
      arg2K0 = 1;
      arg0K1 = 0;
      goto L21702;}
    else {
      arg2K0 = 0;
      arg0K1 = 0;
      goto L21702;}}}
 L21635: {
  env_772X = arg0K0;
  v_773X = AVAILABLEp(3);
  if (v_773X) {
    arg2K0 = 1;
    arg0K1 = env_772X;
    goto L21660;}
  else {
    temp0_774X = collect_saving_temps(env_772X, 1, &temp1_775X);
    v_776X = AVAILABLEp(3);
    if (v_776X) {
      arg2K0 = 1;
      arg0K1 = temp0_774X;
      goto L21660;}
    else {
      arg2K0 = 0;
      arg0K1 = temp0_774X;
      goto L21660;}}}
 L21430: {
  len_777X = ((total_count_410X)<<2);
  addr_778X = ALLOCATE_SPACE(2, (4 + len_777X));
  *((long *) addr_778X) = (10 + (((len_777X)<<8)));
  new_env_779X = 3 + (((long) (addr_778X + 4)));
  v_780X = *((unsigned char *) ((Scode_pointerS) + 1));
  if ((0 == v_780X)) {
    arg0K0 = 0;
    goto L21444;}
  else {
    value_781X = SvalS;
    addr_782X = ((char *) (-3 + new_env_779X));
    S48_WRITE_BARRIER(new_env_779X, addr_782X, value_781X);
    *((long *) addr_782X) = value_781X;
    arg0K0 = 1;
    goto L21444;}}
 L21356: {
  okayP_783X = arg2K0;
  key_784X = arg0K1;
  if (okayP_783X) {
    arg0K0 = key_784X;
    goto L21344;}
  else {
    ps_error("Scheme48 heap overflow", 0);
    arg0K0 = key_784X;
    goto L21344;}}
 L20502: {
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L20539: {
  SstackS = ((((char *) (-3 + (Sbottom_of_stackS)))) + -8);
  *((long *) (((char *) (-3 + (Sbottom_of_stackS))))) = cont_419X;
  ScontS = (Sbottom_of_stackS);
  goto L20502;}
 L20503: {
  if ((1 == cont_419X)) {
    if ((0 == (3 & (SvalS)))) {
      s48_Scallback_return_stack_blockS = 1;
      SstackS = ((((char *) (-3 + (Sbottom_of_stackS)))) + -8);
      *((long *) (((char *) (-3 + (Sbottom_of_stackS))))) = 1;
      ScontS = (Sbottom_of_stackS);
      return ((((SvalS))>>2));}
    else {
      goto L20520;}}
  else {
    goto L20520;}}
 L19863: {
  okayP_785X = arg2K0;
  key_786X = arg0K1;
  if (okayP_785X) {
    arg0K0 = key_786X;
    goto L19818;}
  else {
    ps_error("Scheme48 heap overflow", 0);
    arg0K0 = key_786X;
    goto L19818;}}
 L19775: {
  if ((s48_Spending_interruptPS)) {
    if ((s48_Spending_eventsPS)) {
      s48_Spending_eventsPS = 0;
      check_events_return_tag = 0;
      goto check_events;
     check_events_return_0:
      v_787X = check_events0_return_value;
      if (v_787X) {
        arg0K0 = stack_arg_count_420X;
        goto L17549;}
      else {
        goto L19779;}}
    else {
      arg0K0 = stack_arg_count_420X;
      goto L17549;}}
  else {
    goto L19779;}}
 L16721: {
  stack_arg_count_788X = arg0K0;
  code_789X = *((long *) (((char *) (-3 + (*((long *) (((char *) (-3 + (SvalS))))))))));
  arg0K0 = (*((unsigned char *) ((((char *) (-3 + code_789X))) + 1)));
  arg0K1 = 64;
  goto L16735;}
 L17816: {
  exception_790X = arg0K0;
  stack_arg_count_791X = arg0K1;
  list_args_792X = arg0K2;
  list_arg_count_793X = arg0K3;
  if (((StemplateS) == (SvalS))) {
    if ((0 < (Slosing_opcodeS))) {
      ps_error("wrong number of arguments to exception handler", 1, (Slosing_opcodeS));
      return v_794X;}
    else {
      ps_error("wrong number of arguments to interrupt handler", 1, (0 - (Slosing_opcodeS)));
      return v_795X;}}
  else {
    merged_arg0K0 = list_args_792X;
    merged_arg0K1 = list_arg_count_793X;
    copy_listS_return_tag = 0;
    goto copy_listS;
   copy_listS_return_0:
    v_796X = copy_listS0_return_value;
    merged_arg0K0 = v_796X;
    merged_arg0K1 = stack_arg_count_791X;
    pop_args_GlistS_return_tag = 1;
    goto pop_args_GlistS;
   pop_args_GlistS_return_1:
    args_797X = pop_args_GlistS0_return_value;
    push_exception_continuationB(exception_790X, 0);
    *((long *) (SstackS)) = (SvalS);
    SstackS = ((SstackS) + -4);
    *((long *) (SstackS)) = args_797X;
    SstackS = ((SstackS) + -4);
    arg0K0 = 2;
    goto L17320;}}
 L18283: {
  loc_798X = arg1K0;
  arg_799X = arg1K1;
  if ((top_of_args_431X < arg_799X)) {
    *((long *) loc_798X) = (*((long *) arg_799X));
    arg1K0 = (loc_798X + -4);
    arg1K1 = (arg_799X + -4);
    goto L18283;}
  else {
    SstackS = loc_798X;
    obj_800X = SvalS;
    if ((3 == (3 & obj_800X))) {
      if ((3 == (31 & ((((*((long *) ((((char *) (-3 + obj_800X))) + -4))))>>2))))) {
        arg0K0 = nargs_430X;
        goto L16721;}
      else {
        arg0K0 = 3;
        arg0K1 = nargs_430X;
        arg0K2 = 25;
        arg0K3 = 0;
        goto L17816;}}
    else {
      arg0K0 = 3;
      arg0K1 = nargs_430X;
      arg0K2 = 25;
      arg0K3 = 0;
      goto L17816;}}}
 L18321: {
  stack_arg_count_801X = arg0K0;
  list_args_802X = arg0K1;
  list_arg_count_803X = arg0K2;
  if ((0 == list_arg_count_803X)) {
    obj_804X = SvalS;
    if ((3 == (3 & obj_804X))) {
      if ((3 == (31 & ((((*((long *) ((((char *) (-3 + obj_804X))) + -4))))>>2))))) {
        arg0K0 = stack_arg_count_801X;
        goto L16721;}
      else {
        arg0K0 = 3;
        arg0K1 = stack_arg_count_801X;
        arg0K2 = 25;
        arg0K3 = 0;
        goto L17816;}}
    else {
      arg0K0 = 3;
      arg0K1 = stack_arg_count_801X;
      arg0K2 = 25;
      arg0K3 = 0;
      goto L17816;}}
  else {
    obj_805X = SvalS;
    if ((3 == (3 & obj_805X))) {
      if ((3 == (31 & ((((*((long *) ((((char *) (-3 + obj_805X))) + -4))))>>2))))) {
        code_806X = *((long *) (((char *) (-3 + (*((long *) (((char *) (-3 + (SvalS))))))))));
        total_arg_count_807X = stack_arg_count_801X + list_arg_count_803X;
        arg0K0 = (*((unsigned char *) ((((char *) (-3 + code_806X))) + 1)));
        arg0K1 = 64;
        goto L17012;}
      else {
        arg0K0 = 3;
        arg0K1 = stack_arg_count_801X;
        arg0K2 = list_args_802X;
        arg0K3 = list_arg_count_803X;
        goto L17816;}}
    else {
      arg0K0 = 3;
      arg0K1 = stack_arg_count_801X;
      arg0K2 = list_args_802X;
      arg0K3 = list_arg_count_803X;
      goto L17816;}}}
 L11915: {
  list_args_808X = arg0K0;
  stack_nargs_809X = arg0K1;
  merged_arg0K0 = list_args_808X;
  okay_argument_list_return_tag = 1;
  goto okay_argument_list;
 okay_argument_list_return_1:
  okayP_810X = okay_argument_list0_return_value;
  list_arg_count_811X = okay_argument_list1_return_value;
  if (okayP_810X) {
    SvalS = proc_438X;
    arg0K0 = stack_nargs_809X;
    arg0K1 = list_args_808X;
    arg0K2 = list_arg_count_811X;
    goto L18321;}
  else {
    *((long *) (SstackS)) = list_args_808X;
    SstackS = ((SstackS) + -4);
    merged_arg0K0 = 25;
    merged_arg0K1 = (1 + stack_nargs_809X);
    pop_args_GlistS_return_tag = 2;
    goto pop_args_GlistS;
   pop_args_GlistS_return_2:
    args_812X = pop_args_GlistS0_return_value;
    SstackS = ((SstackS) + 4);
    push_exception_continuationB(5, 0);
    *((long *) (SstackS)) = proc_438X;
    SstackS = ((SstackS) + -4);
    *((long *) (SstackS)) = args_812X;
    SstackS = ((SstackS) + -4);
    arg0K0 = 2;
    goto L17320;}}
 L11956: {
  list_813X = arg0K0;
  follower_814X = arg0K1;
  if ((25 == list_813X)) {
    list_args_815X = *((long *) (((char *) (-3 + (*((long *) ((((char *) (-3 + follower_814X))) + 4)))))));
    addr_816X = (((char *) (-3 + follower_814X))) + 4;
    S48_WRITE_BARRIER(follower_814X, addr_816X, list_args_815X);
    *((long *) addr_816X) = list_args_815X;
    arg0K0 = rest_list_439X;
    arg0K1 = (-1 + stack_nargs_437X);
    goto L11915;}
  else {
    arg0K0 = (*((long *) ((((char *) (-3 + list_813X))) + 4)));
    arg0K1 = (*((long *) ((((char *) (-3 + follower_814X))) + 4)));
    goto L11956;}}
 L18226: {
  obj_817X = SvalS;
  if ((3 == (3 & obj_817X))) {
    if ((3 == (31 & ((((*((long *) ((((char *) (-3 + obj_817X))) + -4))))>>2))))) {
      arg0K0 = 0;
      goto L16721;}
    else {
      arg0K0 = 3;
      arg0K1 = 0;
      arg0K2 = 25;
      arg0K3 = 0;
      goto L17816;}}
  else {
    arg0K0 = 3;
    arg0K1 = 0;
    arg0K2 = 25;
    arg0K3 = 0;
    goto L17816;}}
 L18233: {
  SstackS = ((((char *) (-3 + (Sbottom_of_stackS)))) + -8);
  *((long *) (((char *) (-3 + (Sbottom_of_stackS))))) = cont_440X;
  ScontS = (Sbottom_of_stackS);
  goto L18226;}
 L21091: {
  stack_nargs_818X = arg0K0;
  list_args_819X = arg0K1;
  list_arg_count_820X = arg0K2;
  if ((1 == stack_nargs_818X)) {
    SstackS = ((SstackS) + 4);
    SvalS = (*((long *) (SstackS)));
    pop_continuationB_return_tag = 1;
    goto pop_continuationB;
   pop_continuationB_return_1:
    arg1K0 = (Scode_pointerS);
    goto L19093;}
  else {
    if (((ScontS) == (Sbottom_of_stackS))) {
      arg0K0 = (*((long *) (((char *) (-3 + (Sbottom_of_stackS))))));
      goto L21102;}
    else {
      arg0K0 = (ScontS);
      goto L21102;}}}
 L19599: {
  okayP_821X = arg2K0;
  key_822X = arg0K1;
  if (okayP_821X) {
    arg0K0 = key_822X;
    goto L19587;}
  else {
    ps_error("Scheme48 heap overflow", 0);
    arg0K0 = key_822X;
    goto L19587;}}
 L19472: {
  tem_823X = *((long *) ((((char *) (-3 + (StemplateS)))) + ((((((((*((unsigned char *) ((Scode_pointerS) + 1))))<<8)) + (*((unsigned char *) ((Scode_pointerS) + 2)))))<<2))));
  StemplateS = tem_823X;
  Scode_pointerS = ((((char *) (-3 + (*((long *) (((char *) (-3 + tem_823X)))))))) + 2);
  if ((s48_Spending_interruptPS)) {
    if ((s48_Spending_eventsPS)) {
      s48_Spending_eventsPS = 0;
      check_events_return_tag = 1;
      goto check_events;
     check_events_return_1:
      v_824X = check_events0_return_value;
      if (v_824X) {
        goto L19481;}
      else {
        goto L19484;}}
    else {
      goto L19481;}}
  else {
    goto L19484;}}
 L19655: {
  okayP_825X = arg2K0;
  key_826X = arg0K1;
  if (okayP_825X) {
    arg0K0 = key_826X;
    goto L19642;}
  else {
    ps_error("Scheme48 heap overflow", 0);
    arg0K0 = key_826X;
    goto L19642;}}
 L19517: {
  push_exception_continuationB(5, 4);
  *((long *) (SstackS)) = (*((long *) ((((char *) (-3 + (StemplateS)))) + ((((((((*((unsigned char *) ((Scode_pointerS) + 1))))<<8)) + (*((unsigned char *) ((Scode_pointerS) + 2)))))<<2)))));
  SstackS = ((SstackS) + -4);
  arg0K0 = 1;
  goto L17320;}
 L19373: {
  arg0K0 = (2 + (((max_459X)<<1)));
  goto L19375;}
 L19375: {
  offset_827X = arg0K0;
  Scode_pointerS = ((Scode_pointerS) + offset_827X);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L20405: {
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L19271: {
  delta_828X = arg0K0;
  Scode_pointerS = ((Scode_pointerS) + delta_828X);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L20296: {
  delta_829X = arg0K0;
  Scode_pointerS = ((Scode_pointerS) + delta_829X);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L39115: {
  val_830X = arg0K0;
  SvalS = val_830X;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L39127: {
  val_831X = arg0K0;
  SvalS = val_831X;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L11262: {
  if ((3 == (3 & x_473X))) {
    if ((8 == (31 & ((((*((long *) ((((char *) (-3 + x_473X))) + -4))))>>2))))) {
      arg0K0 = 5;
      goto L39127;}
    else {
      goto L11268;}}
  else {
    goto L11268;}}
 L29202: {
  SvalS = 5;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L29203: {
  if ((3 == (3 & n_474X))) {
    if ((11 == (31 & ((((*((long *) ((((char *) (-3 + n_474X))) + -4))))>>2))))) {
      push_exception_continuationB(5, 1);
      *((long *) (SstackS)) = n_474X;
      SstackS = ((SstackS) + -4);
      arg0K0 = 1;
      goto L17320;}
    else {
      goto L29207;}}
  else {
    goto L29207;}}
 L29359: {
  SvalS = 5;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L29334: {
  if ((3 == (3 & n_475X))) {
    if ((8 == (31 & ((((*((long *) ((((char *) (-3 + n_475X))) + -4))))>>2))))) {
      goto L29359;}
    else {
      goto L29348;}}
  else {
    goto L29348;}}
 L29558: {
  SvalS = 5;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L29533: {
  if ((3 == (3 & n_476X))) {
    if ((8 == (31 & ((((*((long *) ((((char *) (-3 + n_476X))) + -4))))>>2))))) {
      goto L29558;}
    else {
      goto L29547;}}
  else {
    goto L29547;}}
 L29757: {
  SvalS = 5;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L29732: {
  if ((3 == (3 & n_477X))) {
    if ((8 == (31 & ((((*((long *) ((((char *) (-3 + n_477X))) + -4))))>>2))))) {
      goto L29757;}
    else {
      goto L29746;}}
  else {
    goto L29746;}}
 L6998: {
  a_832X = arg0K0;
  if ((b_487X < 0)) {
    arg0K0 = (0 - b_487X);
    goto L7002;}
  else {
    arg0K0 = b_487X;
    goto L7002;}}
 L7279: {
  a_833X = arg0K0;
  if ((b_494X < 0)) {
    arg0K0 = (0 - b_494X);
    goto L7283;}
  else {
    arg0K0 = b_494X;
    goto L7283;}}
 L32718: {
  val_834X = arg0K0;
  SvalS = val_834X;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L32796: {
  val_835X = arg0K0;
  SvalS = val_835X;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L32874: {
  val_836X = arg0K0;
  SvalS = val_836X;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L32952: {
  val_837X = arg0K0;
  SvalS = val_837X;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L33030: {
  val_838X = arg0K0;
  SvalS = val_838X;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L7456: {
  a_839X = arg0K0;
  if ((b_508X < 0)) {
    arg0K0 = (0 - b_508X);
    goto L7460;}
  else {
    arg0K0 = b_508X;
    goto L7460;}}
 L26010: {
  a_840X = arg0K0;
  n_841X = ((y_510X)>>2);
  if ((n_841X < 0)) {
    arg0K0 = (0 - n_841X);
    goto L26012;}
  else {
    arg0K0 = n_841X;
    goto L26012;}}
 L39384: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = x_525X;
  SstackS = ((SstackS) + -4);
  arg0K0 = 1;
  goto L17320;}
 L26296: {
  r_842X = arg0K0;
  if ((536870911 < r_842X)) {
    push_exception_continuationB(5, 1);
    *((long *) (SstackS)) = n_526X;
    SstackS = ((SstackS) + -4);
    arg0K0 = 1;
    goto L17320;}
  else {
    SvalS = (((r_842X)<<2));
    Scode_pointerS = ((Scode_pointerS) + 1);
    arg1K0 = (Scode_pointerS);
    goto L19093;}}
 L26575: {
  SvalS = (((result_547X)<<2));
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L39472: {
  val_843X = arg0K0;
  SvalS = val_843X;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L28977: {
  val_844X = arg0K0;
  SvalS = val_844X;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L28950: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = arg2_550X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  arg0K0 = 2;
  goto L17320;}
 L28887: {
  val_845X = arg0K0;
  SvalS = val_845X;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L28860: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = arg2_552X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  arg0K0 = 2;
  goto L17320;}
 L33722: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = (((x_554X)<<2));
  SstackS = ((SstackS) + -4);
  arg0K0 = 1;
  goto L17320;}
 L39501: {
  val_846X = arg0K0;
  SvalS = val_846X;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L39515: {
  value_847X = arg0K0;
  SvalS = value_847X;
  Scode_pointerS = ((Scode_pointerS) + 2);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L21893: {
  push_exception_continuationB(5, 2);
  *((long *) (SstackS)) = stob_558X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((type_559X)<<2));
  SstackS = ((SstackS) + -4);
  arg0K0 = 2;
  goto L17320;}
 L21965: {
  type_848X = *((unsigned char *) ((Scode_pointerS) + 2));
  len_849X = ((len_560X)<<2);
  addr_850X = ALLOCATE_SPACE(type_848X, (4 + len_849X));
  *((long *) addr_850X) = (2 + (((((((len_849X)<<6)) + type_848X))<<2)));
  new_851X = 3 + (((long) (addr_850X + 4)));
  if ((len_560X < 1)) {
    goto L22000;}
  else {
    *((long *) ((((char *) (-3 + new_851X))) + (-4 + (((len_560X)<<2))))) = (SvalS);
    arg0K0 = (-2 + len_560X);
    goto L21984;}}
 L22147: {
  type_852X = *((unsigned char *) ((Scode_pointerS) + 1));
  len_853X = ((len_565X)<<2);
  addr_854X = ALLOCATE_SPACE(type_852X, (4 + len_853X));
  *((long *) addr_854X) = (2 + (((((((len_853X)<<6)) + type_852X))<<2)));
  new_855X = 3 + (((long) (addr_854X + 4)));
  SstackS = ((SstackS) + 4);
  stack_nargs_856X = (((*((long *) (SstackS))))>>2);
  SstackS = ((SstackS) + 4);
  rest_list_857X = *((long *) (SstackS));
  arg0K0 = (-1 + stack_nargs_856X);
  goto L22169;}
 L22357: {
  push_exception_continuationB(5, 3);
  *((long *) (SstackS)) = stob_570X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((type_572X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((offset_571X)<<2));
  SstackS = ((SstackS) + -4);
  arg0K0 = 3;
  goto L17320;}
 L22460: {
  push_exception_continuationB(5, 3);
  *((long *) (SstackS)) = stob_573X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((type_576X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((offset_575X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = value_574X;
  SstackS = ((SstackS) + -4);
  arg0K0 = 4;
  goto L17320;}
 L22645: {
  push_exception_continuationB(5, 2);
  *((long *) (SstackS)) = (((type_580X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((len_581X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = init_579X;
  SstackS = ((SstackS) + -4);
  arg0K0 = 3;
  goto L17320;}
 L22663: {
  okayP_858X = arg2K0;
  init_859X = arg0K1;
  if (okayP_858X) {
    len_860X = ((len_581X)<<2);
    addr_861X = ALLOCATE_SPACE(type_580X, (4 + len_860X));
    *((long *) addr_861X) = (2 + (((((((len_860X)<<6)) + type_580X))<<2)));
    value_862X = 3 + (((long) (addr_861X + 4)));
    arg0K0 = (-1 + len_581X);
    goto L22692;}
  else {
    push_exception_continuationB(8, 2);
    *((long *) (SstackS)) = (((type_580X)<<2));
    SstackS = ((SstackS) + -4);
    *((long *) (SstackS)) = (((len_581X)<<2));
    SstackS = ((SstackS) + -4);
    *((long *) (SstackS)) = init_859X;
    SstackS = ((SstackS) + -4);
    arg0K0 = 3;
    goto L17320;}}
 L22941: {
  push_exception_continuationB(7, 2);
  *((long *) (SstackS)) = stob_587X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((type_589X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = index_588X;
  SstackS = ((SstackS) + -4);
  arg0K0 = 3;
  goto L17320;}
 L22918: {
  push_exception_continuationB(5, 2);
  *((long *) (SstackS)) = stob_587X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((type_589X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = index_588X;
  SstackS = ((SstackS) + -4);
  arg0K0 = 3;
  goto L17320;}
 L23181: {
  push_exception_continuationB(7, 2);
  *((long *) (SstackS)) = stob_593X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((type_595X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = arg2_592X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = value_594X;
  SstackS = ((SstackS) + -4);
  arg0K0 = 4;
  goto L17320;}
 L23156: {
  push_exception_continuationB(5, 2);
  *((long *) (SstackS)) = stob_593X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((type_595X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = arg2_592X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = value_594X;
  SstackS = ((SstackS) + -4);
  arg0K0 = 4;
  goto L17320;}
 L26746: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = (((len_600X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((init_601X)<<2));
  SstackS = ((SstackS) + -4);
  arg0K0 = 2;
  goto L17320;}
 L26838: {
  addr_863X = ALLOCATE_SPACE(18, (4 + len_600X));
  *((long *) addr_863X) = (74 + (((len_600X)<<8)));
  vector_864X = 3 + (((long) (addr_863X + 4)));
  arg0K0 = (-1 + len_600X);
  goto L26787;}
 L37292: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  arg0K0 = 1;
  goto L17320;}
 L33828: {
  push_exception_continuationB(7, 1);
  *((long *) (SstackS)) = arg2_607X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((index_608X)<<2));
  SstackS = ((SstackS) + -4);
  arg0K0 = 2;
  goto L17320;}
 L36868: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = arg2_607X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  arg0K0 = 2;
  goto L17320;}
 L30738: {
  push_exception_continuationB(7, 1);
  *((long *) (SstackS)) = arg3_611X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((index_612X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((Kchar_613X)<<2));
  SstackS = ((SstackS) + -4);
  arg0K0 = 3;
  goto L17320;}
 L30717: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = arg3_611X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((index_612X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((Kchar_613X)<<2));
  SstackS = ((SstackS) + -4);
  arg0K0 = 3;
  goto L17320;}
 L35280: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = arg3_611X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = arg2_610X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  arg0K0 = 3;
  goto L17320;}
 L26980: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = (((len_616X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (9 + ((((init_617X))<<8)));
  SstackS = ((SstackS) + -4);
  arg0K0 = 2;
  goto L17320;}
 L27072: {
  len_865X = 1 + len_616X;
  addr_866X = ALLOCATE_SPACE(17, (4 + len_865X));
  *((long *) addr_866X) = (70 + (((len_865X)<<8)));
  string_867X = 3 + (((long) (addr_866X + 4)));
  *((unsigned char *) ((((char *) (-3 + string_867X))) + len_616X)) = 0;
  arg0K0 = (-1 + len_616X);
  goto L27021;}
 L33909: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = arg2_615X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  arg0K0 = 2;
  goto L17320;}
 L37342: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  arg0K0 = 1;
  goto L17320;}
 L33972: {
  push_exception_continuationB(7, 1);
  *((long *) (SstackS)) = arg2_623X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((index_624X)<<2));
  SstackS = ((SstackS) + -4);
  arg0K0 = 2;
  goto L17320;}
 L36964: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = arg2_623X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  arg0K0 = 2;
  goto L17320;}
 L30893: {
  push_exception_continuationB(7, 1);
  *((long *) (SstackS)) = arg3_627X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((index_628X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (9 + ((((Kchar_629X))<<8)));
  SstackS = ((SstackS) + -4);
  arg0K0 = 3;
  goto L17320;}
 L30872: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = arg3_627X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((index_628X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (9 + ((((Kchar_629X))<<8)));
  SstackS = ((SstackS) + -4);
  arg0K0 = 3;
  goto L17320;}
 L35406: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = arg3_627X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = arg2_626X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  arg0K0 = 3;
  goto L17320;}
 L25150: {
  obj_868X = SvalS;
  if ((3 == (3 & obj_868X))) {
    if ((17 == (31 & ((((*((long *) ((((char *) (-3 + obj_868X))) + -4))))>>2))))) {
      table_869X = Sthe_symbol_tableS;
      string_870X = SvalS;
      n_871X = -1 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + string_870X))) + -4))))>>8));
      arg0K0 = 0;
      arg0K1 = 0;
      goto L13975;}
    else {
      goto L25160;}}
  else {
    goto L25160;}}
 L34953: {
  val_872X = arg0K0;
  SvalS = val_872X;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L34934: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  arg0K0 = 1;
  goto L17320;}
 L30480: {
  if ((1 == (SvalS))) {
    addr_873X = (((char *) (-3 + arg2_637X))) + 4;
    S48_WRITE_BARRIER(arg2_637X, addr_873X, 273);
    *((long *) addr_873X) = 273;
    goto L30486;}
  else {
    if ((17 == (255 & (*((long *) ((((char *) (-3 + arg2_637X))) + 4)))))) {
      addr_874X = (((char *) (-3 + arg2_637X))) + 4;
      S48_WRITE_BARRIER(arg2_637X, addr_874X, 529);
      *((long *) addr_874X) = 529;
      goto L30486;}
    else {
      goto L30486;}}}
 L30487: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = arg2_637X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  arg0K0 = 2;
  goto L17320;}
 L39587: {
  val_875X = arg0K0;
  SvalS = val_875X;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L34054: {
  SvalS = x_640X;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L34105: {
  okayP_876X = arg2K0;
  key_877X = arg0K1;
  if (okayP_876X) {
    arg0K0 = key_877X;
    goto L34071;}
  else {
    ps_error("Scheme48 heap overflow", 0);
    arg0K0 = key_877X;
    goto L34071;}}
 L35524: {
  okayP_878X = arg2K0;
  key_879X = arg0K1;
  if (okayP_878X) {
    arg0K0 = key_879X;
    goto L35506;}
  else {
    ps_error("Scheme48 heap overflow", 0);
    arg0K0 = key_879X;
    goto L35506;}}
 L38430: {
  okayP_880X = arg2K0;
  key_881X = arg0K1;
  if (okayP_880X) {
    arg0K0 = key_881X;
    goto L38360;}
  else {
    ps_error("Scheme48 heap overflow", 0);
    arg0K0 = key_881X;
    goto L38360;}}
 L38690: {
  okayP_882X = arg2K0;
  key_883X = arg0K1;
  if (okayP_882X) {
    arg0K0 = key_883X;
    goto L38632;}
  else {
    ps_error("Scheme48 heap overflow", 0);
    arg0K0 = key_883X;
    goto L38632;}}
 L28629: {
  if ((1 == (SvalS))) {
    arg2K0 = 0;
    goto L28633;}
  else {
    arg2K0 = 1;
    goto L28633;}}
 L28636: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = arg2_653X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  arg0K0 = 2;
  goto L17320;}
 L35009: {
  okayP_884X = arg2K0;
  key_885X = arg0K1;
  if (okayP_884X) {
    arg0K0 = key_885X;
    goto L34991;}
  else {
    ps_error("Scheme48 heap overflow", 0);
    arg0K0 = key_885X;
    goto L34991;}}
 L8509: {
  inputP_886X = arg2K0;
  if (inputP_886X) {
    arg0K0 = (Spending_input_channels_headS);
    goto L8517;}
  else {
    arg0K0 = (Spending_output_channels_headS);
    goto L8517;}}
 L34159: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  arg0K0 = 1;
  goto L17320;}
 L15425: {
  arg0K0 = (-1 + (Snumber_of_channelsS));
  arg0K1 = 25;
  goto L15432;}
 L35625: {
  okayP_887X = arg2K0;
  key_888X = arg0K1;
  if (okayP_887X) {
    arg0K0 = key_888X;
    goto L35579;}
  else {
    ps_error("Scheme48 heap overflow", 0);
    arg0K0 = key_888X;
    goto L35579;}}
 L36457: {
  i_889X = arg0K0;
  h_890X = arg0K1;
  if ((i_889X < n_672X)) {
    arg0K0 = (1 + i_889X);
    arg0K1 = (h_890X + (((*((unsigned char *) ((((char *) (-3 + x_671X))) + i_889X))))));
    goto L36457;}
  else {
    SvalS = (((h_890X)<<2));
    Scode_pointerS = ((Scode_pointerS) + 1);
    arg1K0 = (Scode_pointerS);
    goto L19093;}}
 L36434: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  arg0K0 = 1;
  goto L17320;}
 L39657: {
  SstackS = ((SstackS) + 4);
  stob_891X = *((long *) (SstackS));
  proc_892X = SvalS;
  if ((3 == (3 & stob_891X))) {
    if ((3 == (3 & proc_892X))) {
      if ((3 == (31 & ((((*((long *) ((((char *) (-3 + proc_892X))) + -4))))>>2))))) {
        addr_893X = ALLOCATE_SPACE(0, 12);
        *((long *) addr_893X) = 2050;
        x_894X = 3 + (((long) (addr_893X + 4)));
        *((long *) (((char *) (-3 + x_894X)))) = stob_891X;
        *((long *) ((((char *) (-3 + x_894X))) + 4)) = proc_892X;
        b_895X = Sfinalizer_alistS;
        addr_896X = ALLOCATE_SPACE(0, 12);
        *((long *) addr_896X) = 2050;
        x_897X = 3 + (((long) (addr_896X + 4)));
        *((long *) (((char *) (-3 + x_897X)))) = x_894X;
        *((long *) ((((char *) (-3 + x_897X))) + 4)) = b_895X;
        Sfinalizer_alistS = x_897X;
        SvalS = 13;
        Scode_pointerS = ((Scode_pointerS) + 1);
        arg1K0 = (Scode_pointerS);
        goto L19093;}
      else {
        goto L31537;}}
    else {
      goto L31537;}}
  else {
    goto L31537;}}
 L34267: {
  firstP_898X = arg2K0;
  vector_899X = s48_find_all(type_682X);
  if ((1 == vector_899X)) {
    if (firstP_898X) {
      collect_saving_temps(0, 0, &v_900X);
      arg2K0 = 0;
      goto L34267;}
    else {
      push_exception_continuationB(8, 1);
      *((long *) (SstackS)) = (((type_682X)<<2));
      SstackS = ((SstackS) + -4);
      arg0K0 = 1;
      goto L17320;}}
  else {
    SvalS = vector_899X;
    Scode_pointerS = ((Scode_pointerS) + 1);
    arg1K0 = (Scode_pointerS);
    goto L19093;}}
 L39707: {
  firstP_901X = arg2K0;
  type_902X = arg0K1;
  vector_903X = s48_find_all_records(type_902X);
  if ((1 == vector_903X)) {
    if (firstP_901X) {
      merged_arg0K0 = type_902X;
      collect_saving_temp_return_tag = 0;
      goto collect_saving_temp;
     collect_saving_temp_return_0:
      v_904X = collect_saving_temp0_return_value;
      arg2K0 = 0;
      arg0K1 = v_904X;
      goto L39707;}
    else {
      push_exception_continuationB(8, 1);
      *((long *) (SstackS)) = type_902X;
      SstackS = ((SstackS) + -4);
      arg0K0 = 1;
      goto L17320;}}
  else {
    SvalS = vector_903X;
    Scode_pointerS = ((Scode_pointerS) + 1);
    arg1K0 = (Scode_pointerS);
    goto L19093;}}
 L21234: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  arg0K0 = 1;
  goto L17320;}
 L20149: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  arg0K0 = 1;
  goto L17320;}
 L39748: {
  SvalS = (((old_691X)<<2));
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L19107: {
  SstackS = ((SstackS) + 4);
  pc_905X = *((long *) (SstackS));
  SstackS = ((SstackS) + 4);
  tem_906X = *((long *) (SstackS));
  StemplateS = tem_906X;
  Scode_pointerS = ((((char *) (-3 + (*((long *) (((char *) (-3 + tem_906X)))))))) + (((pc_905X)>>2)));
  SstackS = ((SstackS) + 4);
  SvalS = (*((long *) (SstackS)));
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L38250: {
  x_907X = s48_schedule_alarm_interrupt((((p_694X)>>2)));
  SvalS = (((x_907X)<<2));
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L35153: {
  if ((1 == (SvalS))) {
    arg2K0 = 0;
    goto L35157;}
  else {
    arg2K0 = 1;
    goto L35157;}}
 L35158: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = arg2_695X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  arg0K0 = 2;
  goto L17320;}
 L23453: {
  rest_list_908X = arg0K0;
  if ((25 == rest_list_908X)) {
    proc_909X = *((long *) ((SstackS) + (((nargs_697X)<<2))));
    name_910X = *((long *) ((SstackS) + (-4 + (((nargs_697X)<<2)))));
    args_911X = (SstackS) + 4;
    *((long *) (SstackS)) = (10 + (((nargs_697X)<<10)));
    SstackS = ((SstackS) + -4);
    if ((3 == (3 & name_910X))) {
      if ((17 == (31 & ((((*((long *) ((((char *) (-3 + name_910X))) + -4))))>>2))))) {
        if ((3 == (3 & proc_909X))) {
          if ((18 == (31 & ((((*((long *) ((((char *) (-3 + proc_909X))) + -4))))>>2))))) {
            if ((4 == ((long)(((unsigned long)(*((long *) ((((char *) (-3 + proc_909X))) + -4))))>>8)))) {
              result_912X = s48_external_call(proc_909X, name_910X, (-2 + nargs_697X), args_911X);
              if ((Sexternal_exceptionPS)) {
                Sexternal_exceptionPS = 0;
                arg0K0 = (Sexternal_exception_nargsS);
                goto L17320;}
              else {
                SvalS = result_912X;
                Scode_pointerS = ((Scode_pointerS) + 1);
                arg1K0 = (Scode_pointerS);
                goto L19093;}}
            else {
              goto L23513;}}
          else {
            goto L23513;}}
        else {
          goto L23513;}}
      else {
        goto L23513;}}
    else {
      goto L23513;}}
  else {
    *((long *) (SstackS)) = (*((long *) (((char *) (-3 + rest_list_908X)))));
    SstackS = ((SstackS) + -4);
    arg0K0 = (*((long *) ((((char *) (-3 + rest_list_908X))) + 4)));
    goto L23453;}}
 L34376: {
  okayP_913X = arg2K0;
  key_914X = arg0K1;
  if (okayP_913X) {
    arg0K0 = key_914X;
    goto L34342;}
  else {
    ps_error("Scheme48 heap overflow", 0);
    arg0K0 = key_914X;
    goto L34342;}}
 L24577: {
  if ((1 == (SvalS))) {
    arg0K0 = (Sexported_bindingsS);
    goto L24610;}
  else {
    arg0K0 = (Simported_bindingsS);
    goto L24610;}}
 L24582: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = arg2_703X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  arg0K0 = 2;
  goto L17320;}
 L36368: {
  option_915X = arg0K0;
  seconds_916X = arg0K1;
  mseconds_917X = arg0K2;
  if ((536869 < seconds_916X)) {
    push_exception_continuationB(6, 1);
    *((long *) (SstackS)) = (((option_915X)<<2));
    SstackS = ((SstackS) + -4);
    *((long *) (SstackS)) = (((seconds_916X)<<2));
    SstackS = ((SstackS) + -4);
    *((long *) (SstackS)) = (((mseconds_917X)<<2));
    SstackS = ((SstackS) + -4);
    arg0K0 = 3;
    goto L17320;}
  else {
    SvalS = (((((1000 * seconds_916X) + mseconds_917X))<<2));
    Scode_pointerS = ((Scode_pointerS) + 1);
    arg1K0 = (Scode_pointerS);
    goto L19093;}}
 L39808: {
  s48_Scallback_return_stack_blockS = arg2_717X;
  return x_718X;}
 L28527: {
  val_918X = arg0K0;
  SvalS = val_918X;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L28494: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = arg2_719X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  arg0K0 = 2;
  goto L17320;}
 L34585: {
  SstackS = ((SstackS) + 4);
  arg2_919X = *((long *) (SstackS));
  if ((0 == (3 & (SvalS)))) {
    n_920X = (((SvalS))>>2);
    if ((3 == (3 & arg2_919X))) {
      if ((0 == (31 & ((((*((long *) ((((char *) (-3 + arg2_919X))) + -4))))>>2))))) {
        goto L28054;}
      else {
        goto L28006;}}
    else {
      goto L28006;}}
  else {
    push_exception_continuationB(5, 1);
    *((long *) (SstackS)) = arg2_919X;
    SstackS = ((SstackS) + -4);
    *((long *) (SstackS)) = (SvalS);
    SstackS = ((SstackS) + -4);
    arg0K0 = 2;
    goto L17320;}}
 L30135: {
  list_921X = arg0K0;
  slow_922X = arg0K1;
  move_slowP_923X = arg2K2;
  if ((25 == list_921X)) {
    SvalS = 1;
    Scode_pointerS = ((Scode_pointerS) + 1);
    arg1K0 = (Scode_pointerS);
    goto L19093;}
  else {
    if ((3 == (3 & list_921X))) {
      if ((0 == (31 & ((((*((long *) ((((char *) (-3 + list_921X))) + -4))))>>2))))) {
        head_924X = *((long *) (((char *) (-3 + list_921X))));
        if ((3 == (3 & head_924X))) {
          if ((0 == (31 & ((((*((long *) ((((char *) (-3 + head_924X))) + -4))))>>2))))) {
            if (((*((long *) (((char *) (-3 + head_924X))))) == thing_727X)) {
              SvalS = head_924X;
              Scode_pointerS = ((Scode_pointerS) + 1);
              arg1K0 = (Scode_pointerS);
              goto L19093;}
            else {
              list_925X = *((long *) ((((char *) (-3 + list_921X))) + 4));
              if ((list_925X == slow_922X)) {
                push_exception_continuationB(5, 1);
                *((long *) (SstackS)) = thing_727X;
                SstackS = ((SstackS) + -4);
                *((long *) (SstackS)) = list_728X;
                SstackS = ((SstackS) + -4);
                arg0K0 = 2;
                goto L17320;}
              else {
                if (move_slowP_923X) {
                  arg0K0 = list_925X;
                  arg0K1 = (*((long *) ((((char *) (-3 + slow_922X))) + 4)));
                  arg2K2 = 0;
                  goto L30135;}
                else {
                  arg0K0 = list_925X;
                  arg0K1 = slow_922X;
                  arg2K2 = 1;
                  goto L30135;}}}}
          else {
            push_exception_continuationB(5, 1);
            *((long *) (SstackS)) = thing_727X;
            SstackS = ((SstackS) + -4);
            *((long *) (SstackS)) = list_728X;
            SstackS = ((SstackS) + -4);
            arg0K0 = 2;
            goto L17320;}}
        else {
          push_exception_continuationB(5, 1);
          *((long *) (SstackS)) = thing_727X;
          SstackS = ((SstackS) + -4);
          *((long *) (SstackS)) = list_728X;
          SstackS = ((SstackS) + -4);
          arg0K0 = 2;
          goto L17320;}}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = thing_727X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = list_728X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17320;}}
    else {
      push_exception_continuationB(5, 1);
      *((long *) (SstackS)) = thing_727X;
      SstackS = ((SstackS) + -4);
      *((long *) (SstackS)) = list_728X;
      SstackS = ((SstackS) + -4);
      arg0K0 = 2;
      goto L17320;}}}
 L28200: {
  push_exception_continuationB(7, 1);
  *((long *) (SstackS)) = arg3_730X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = arg2_729X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((index_731X)<<2));
  SstackS = ((SstackS) + -4);
  arg0K0 = 3;
  goto L17320;}
 L28220: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = arg3_730X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = arg2_729X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((index_731X)<<2));
  SstackS = ((SstackS) + -4);
  arg0K0 = 3;
  goto L17320;}
 L31678: {
  push_exception_continuationB(7, 1);
  *((long *) (SstackS)) = arg4_735X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = arg3_734X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((index_736X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = value_737X;
  SstackS = ((SstackS) + -4);
  arg0K0 = 4;
  goto L17320;}
 L31700: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = arg4_735X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = arg3_734X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((index_736X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = value_737X;
  SstackS = ((SstackS) + -4);
  arg0K0 = 4;
  goto L17320;}
 L31929: {
  if ((3 == (3 & arg5_743X))) {
    if ((17 == (31 & ((((*((long *) ((((char *) (-3 + arg5_743X))) + -4))))>>2))))) {
      goto L31942;}
    else {
      goto L31937;}}
  else {
    goto L31937;}}
 L31924: {
  if ((3 == (3 & arg5_743X))) {
    if ((18 == (31 & ((((*((long *) ((((char *) (-3 + arg5_743X))) + -4))))>>2))))) {
      goto L31929;}
    else {
      goto L31996;}}
  else {
    goto L31996;}}
 L23709: {
  port_926X = arg0K0;
  if ((3 == (3 & port_926X))) {
    if ((7 == (31 & ((((*((long *) ((((char *) (-3 + port_926X))) + -4))))>>2))))) {
      if ((0 == (4 & ((((*((long *) ((((char *) (-3 + port_926X))) + 4))))>>2))))) {
        goto L23769;}
      else {
        p_927X = *((long *) ((((char *) (-3 + port_926X))) + 24));
        p_928X = *((long *) ((((char *) (-3 + port_926X))) + 28));
        b_929X = *((long *) ((((char *) (-3 + port_926X))) + 20));
        i_930X = ((p_927X)>>2);
        x_931X = *((long *) ((((char *) (-3 + port_926X))) + 12));
        if ((5 == x_931X)) {
          goto L23749;}
        else {
          if ((i_930X == (((p_928X)>>2)))) {
            goto L23749;}
          else {
            val_932X = 4 + (((i_930X)<<2));
            addr_933X = (((char *) (-3 + port_926X))) + 24;
            S48_WRITE_BARRIER(port_926X, addr_933X, val_932X);
            *((long *) addr_933X) = val_932X;
            SvalS = (9 + ((((((*((unsigned char *) ((((char *) (-3 + b_929X))) + i_930X))))))<<8)));
            Scode_pointerS = ((Scode_pointerS) + 2);
            arg1K0 = (Scode_pointerS);
            goto L19093;}}}}
    else {
      goto L23769;}}
  else {
    goto L23769;}}
 L23929: {
  port_934X = arg0K0;
  if ((3 == (3 & port_934X))) {
    if ((7 == (31 & ((((*((long *) ((((char *) (-3 + port_934X))) + -4))))>>2))))) {
      if ((0 == (4 & ((((*((long *) ((((char *) (-3 + port_934X))) + 4))))>>2))))) {
        goto L23989;}
      else {
        p_935X = *((long *) ((((char *) (-3 + port_934X))) + 24));
        p_936X = *((long *) ((((char *) (-3 + port_934X))) + 28));
        b_937X = *((long *) ((((char *) (-3 + port_934X))) + 20));
        i_938X = ((p_935X)>>2);
        x_939X = *((long *) ((((char *) (-3 + port_934X))) + 12));
        if ((5 == x_939X)) {
          goto L23969;}
        else {
          if ((i_938X == (((p_936X)>>2)))) {
            goto L23969;}
          else {
            SvalS = (9 + ((((((*((unsigned char *) ((((char *) (-3 + b_937X))) + i_938X))))))<<8)));
            Scode_pointerS = ((Scode_pointerS) + 2);
            arg1K0 = (Scode_pointerS);
            goto L19093;}}}}
    else {
      goto L23989;}}
  else {
    goto L23989;}}
 L24142: {
  Kchar_940X = arg0K0;
  port_941X = arg0K1;
  if ((9 == (255 & Kchar_940X))) {
    if ((3 == (3 & port_941X))) {
      if ((7 == (31 & ((((*((long *) ((((char *) (-3 + port_941X))) + -4))))>>2))))) {
        if ((0 == (8 & ((((*((long *) ((((char *) (-3 + port_941X))) + 4))))>>2))))) {
          goto L24203;}
        else {
          p_942X = *((long *) ((((char *) (-3 + port_941X))) + 24));
          b_943X = *((long *) ((((char *) (-3 + port_941X))) + 20));
          i_944X = ((p_942X)>>2);
          x_945X = *((long *) ((((char *) (-3 + port_941X))) + 12));
          if ((5 == x_945X)) {
            goto L24185;}
          else {
            if ((i_944X == ((long)(((unsigned long)(*((long *) ((((char *) (-3 + b_943X))) + -4))))>>8)))) {
              goto L24185;}
            else {
              val_946X = 4 + (((i_944X)<<2));
              addr_947X = (((char *) (-3 + port_941X))) + 24;
              S48_WRITE_BARRIER(port_941X, addr_947X, val_946X);
              *((long *) addr_947X) = val_946X;
              *((unsigned char *) ((((char *) (-3 + (*((long *) ((((char *) (-3 + port_941X))) + 20)))))) + i_944X)) = (((((Kchar_940X)>>8))));
              SvalS = 13;
              Scode_pointerS = ((Scode_pointerS) + 2);
              arg1K0 = (Scode_pointerS);
              goto L19093;}}}}
      else {
        goto L24203;}}
    else {
      goto L24203;}}
  else {
    goto L24203;}}
 L30388: {
  stuff_948X = arg0K0;
  if ((3 == (3 & stuff_948X))) {
    if ((0 == (31 & ((((*((long *) ((((char *) (-3 + stuff_948X))) + -4))))>>2))))) {
      thing_949X = *((long *) (((char *) (-3 + stuff_948X))));
      if ((0 == (3 & thing_949X))) {
        ps_write_integer((((thing_949X)>>2)), out_755X);
        goto L30394;}
      else {
        if ((9 == (255 & thing_949X))) {
          ps_write_string("#\\", out_755X);
          { long ignoreXX;
          PS_WRITE_CHAR(((((thing_949X)>>8))), out_755X, ignoreXX) }
          goto L30394;}
        else {
          if ((3 == (3 & thing_949X))) {
            if ((9 == (31 & ((((*((long *) ((((char *) (-3 + thing_949X))) + -4))))>>2))))) {
              if ((0 < ((((3 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + thing_949X))) + -4))))>>8))))>>2)))) {
                type_950X = *((long *) (((char *) (-3 + thing_949X))));
                if ((3 == (3 & type_950X))) {
                  if ((9 == (31 & ((((*((long *) ((((char *) (-3 + type_950X))) + -4))))>>2))))) {
                    if ((2 < ((((3 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + type_950X))) + -4))))>>8))))>>2)))) {
                      obj_951X = *((long *) ((((char *) (-3 + type_950X))) + 8));
                      if ((3 == (3 & obj_951X))) {
                        if ((1 == (31 & ((((*((long *) ((((char *) (-3 + obj_951X))) + -4))))>>2))))) {
                          ps_write_string("#{", out_755X);
                          ps_write_string((((char *)(((char *) (-3 + (*((long *) (((char *) (-3 + (*((long *) ((((char *) (-3 + (*((long *) (((char *) (-3 + thing_949X)))))))) + 8))))))))))))), out_755X);
                          { long ignoreXX;
                          PS_WRITE_CHAR(125, out_755X, ignoreXX) }
                          goto L30394;}
                        else {
                          goto L12292;}}
                      else {
                        goto L12292;}}
                    else {
                      goto L12292;}}
                  else {
                    goto L12292;}}
                else {
                  goto L12292;}}
              else {
                goto L12292;}}
            else {
              goto L12292;}}
          else {
            goto L12292;}}}}
    else {
      goto L30379;}}
  else {
    goto L30379;}}
 L17379: {
  SvalS = (*((long *) ((((char *) (-3 + (Sexception_handlersS)))) + (((opcode_757X)<<2)))));
  obj_952X = SvalS;
  if ((3 == (3 & obj_952X))) {
    if ((3 == (31 & ((((*((long *) ((((char *) (-3 + obj_952X))) + -4))))>>2))))) {
      goto L17396;}
    else {
      goto L17466;}}
  else {
    goto L17466;}}
 L17452: {
  merged_arg3K0 = "exception-handlers is not a vector";
  loseD0_return_tag = 0;
  goto loseD0;
 loseD0_return_0:
  goto L17379;}
 L21702: {
  okayP_953X = arg2K0;
  key_954X = arg0K1;
  if (okayP_953X) {
    arg0K0 = key_954X;
    goto L21630;}
  else {
    ps_error("Scheme48 heap overflow", 0);
    arg0K0 = key_954X;
    goto L21630;}}
 L21660: {
  okayP_955X = arg2K0;
  temp_956X = arg0K1;
  if (okayP_955X) {
    arg0K0 = temp_956X;
    goto L21641;}
  else {
    ps_error("Scheme48 heap overflow", 0);
    arg0K0 = temp_956X;
    goto L21641;}}
 L21444: {
  start_i_957X = arg0K0;
  arg0K0 = start_i_957X;
  arg0K1 = 2;
  arg0K2 = (SenvS);
  goto L21452;}
 L21344: {
  key_958X = arg0K0;
  if ((1 == (ScontS))) {
    arg0K0 = 1;
    goto L21348;}
  else {
    merged_arg0K0 = key_958X;
    merged_arg0K1 = 2;
    really_preserve_continuation_return_tag = 0;
    goto really_preserve_continuation;
   really_preserve_continuation_return_0:
    v_959X = really_preserve_continuation0_return_value;
    arg0K0 = v_959X;
    goto L21348;}}
 L20520: {
  SstackS = ((((char *) (-3 + (Sbottom_of_stackS)))) + -8);
  *((long *) (((char *) (-3 + (Sbottom_of_stackS))))) = 1;
  ScontS = (Sbottom_of_stackS);
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = cont_419X;
  SstackS = ((SstackS) + -4);
  arg0K0 = 2;
  goto L17320;}
 L19818: {
  v_960X = arg0K0;
  merged_arg0K0 = v_960X;
  copy_stack_into_heap_return_tag = 0;
  goto copy_stack_into_heap;
 copy_stack_into_heap_return_0:
  if (((SstackS) < (Sstack_limitS))) {
    ps_error("Couldn't get default procedure space (how can this happen?)", 0);
    goto L19775;}
  else {
    goto L19775;}}
 L17549: {
  stack_arg_count_961X = arg0K0;
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (StemplateS);
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((((Scode_pointerS) - (((char *) (-3 + (*((long *) (((char *) (-3 + (StemplateS)))))))))))<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = ((((Senabled_interruptsS))<<2));
  SstackS = ((SstackS) + -4);
  tem_962X = Sinterrupt_templateS;
  StemplateS = tem_962X;
  Scode_pointerS = (((char *) (-3 + (*((long *) (((char *) (-3 + tem_962X))))))));
  push_continuationB((Scode_pointerS), (4 + stack_arg_count_961X));
  n_963X = (Spending_interruptsS) & (Senabled_interruptsS);
  arg0K0 = 0;
  arg0K1 = 1;
  goto L17654;}
 L19779: {
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L16735: {
  protocol_964X = arg0K0;
  stack_space_965X = arg0K1;
  if ((68 == protocol_964X)) {
    if ((stack_arg_count_788X < 3)) {
      skip_966X = *((unsigned char *) ((((char *) (-3 + code_789X))) + (2 + stack_arg_count_788X)));
      if ((0 == skip_966X)) {
        arg0K0 = 4;
        arg0K1 = stack_arg_count_788X;
        arg0K2 = 25;
        arg0K3 = 0;
        goto L17816;}
      else {
        arg0K0 = skip_966X;
        arg0K1 = stack_arg_count_788X;
        goto L16739;}}
    else {
      skip_967X = *((unsigned char *) ((((char *) (-3 + code_789X))) + 5));
      if ((0 == skip_967X)) {
        arg0K0 = 4;
        arg0K1 = stack_arg_count_788X;
        arg0K2 = 25;
        arg0K3 = 0;
        goto L17816;}
      else {
        arg0K0 = skip_967X;
        goto L16762;}}}
  else {
    if ((67 == protocol_964X)) {
      if ((stack_arg_count_788X < (*((unsigned char *) ((((char *) (-3 + code_789X))) + 2))))) {
        arg0K0 = 4;
        arg0K1 = stack_arg_count_788X;
        arg0K2 = 25;
        arg0K3 = 0;
        goto L17816;}
      else {
        arg0K0 = 3;
        goto L16762;}}
    else {
      if ((65 == protocol_964X)) {
        wants_stack_args_968X = ((((*((unsigned char *) ((((char *) (-3 + code_789X))) + 2))))<<8)) + (*((unsigned char *) ((((char *) (-3 + code_789X))) + 3)));
        if ((stack_arg_count_788X < wants_stack_args_968X)) {
          arg0K0 = 4;
          arg0K1 = stack_arg_count_788X;
          arg0K2 = 25;
          arg0K3 = 0;
          goto L17816;}
        else {
          merged_arg0K0 = wants_stack_args_968X;
          merged_arg0K1 = stack_arg_count_788X;
          merged_arg0K2 = 25;
          merged_arg0K3 = 0;
          rest_list_setup_return_tag = 0;
          goto rest_list_setup;
         rest_list_setup_return_0:
          arg0K0 = 4;
          arg0K1 = (1 + wants_stack_args_968X);
          goto L16739;}}
      else {
        if ((63 < protocol_964X)) {
          if ((64 == protocol_964X)) {
            if (((((((*((unsigned char *) ((((char *) (-3 + code_789X))) + 2))))<<8)) + (*((unsigned char *) ((((char *) (-3 + code_789X))) + 3)))) == stack_arg_count_788X)) {
              arg0K0 = 4;
              arg0K1 = stack_arg_count_788X;
              goto L16739;}
            else {
              arg0K0 = 4;
              arg0K1 = stack_arg_count_788X;
              arg0K2 = 25;
              arg0K3 = 0;
              goto L17816;}}
          else {
            if ((66 == protocol_964X)) {
              length_969X = (long)(((unsigned long)(*((long *) ((((char *) (-3 + code_789X))) + -4))))>>8);
              index_970X = -2 + length_969X;
              arg0K0 = (*((unsigned char *) ((((char *) (-3 + code_789X))) + (-3 + length_969X))));
              arg0K1 = (((((*((unsigned char *) ((((char *) (-3 + code_789X))) + index_970X))))<<8)) + (*((unsigned char *) ((((char *) (-3 + code_789X))) + (1 + index_970X)))));
              goto L16735;}
            else {
              ps_error("unknown protocol", 1, protocol_964X);
              arg0K0 = 4;
              arg0K1 = stack_arg_count_788X;
              arg0K2 = 25;
              arg0K3 = 0;
              goto L17816;}}}
        else {
          if ((protocol_964X == stack_arg_count_788X)) {
            arg0K0 = 2;
            arg0K1 = stack_arg_count_788X;
            goto L16739;}
          else {
            arg0K0 = 4;
            arg0K1 = stack_arg_count_788X;
            arg0K2 = 25;
            arg0K3 = 0;
            goto L17816;}}}}}}
 L17012: {
  protocol_971X = arg0K0;
  stack_space_972X = arg0K1;
  if ((68 == protocol_971X)) {
    if ((total_arg_count_807X < 3)) {
      skip_973X = *((unsigned char *) ((((char *) (-3 + code_806X))) + (2 + total_arg_count_807X)));
      if ((0 == skip_973X)) {
        arg0K0 = 4;
        arg0K1 = stack_arg_count_801X;
        arg0K2 = list_args_802X;
        arg0K3 = list_arg_count_803X;
        goto L17816;}
      else {
        merged_arg0K0 = list_args_802X;
        merged_arg0K1 = list_arg_count_803X;
        push_list_return_tag = 0;
        goto push_list;
       push_list_return_0:
        arg0K0 = skip_973X;
        arg0K1 = total_arg_count_807X;
        goto L17016;}}
    else {
      skip_974X = *((unsigned char *) ((((char *) (-3 + code_806X))) + 5));
      if ((0 == skip_974X)) {
        arg0K0 = 4;
        arg0K1 = stack_arg_count_801X;
        arg0K2 = list_args_802X;
        arg0K3 = list_arg_count_803X;
        goto L17816;}
      else {
        arg0K0 = skip_974X;
        goto L17041;}}}
  else {
    if ((67 == protocol_971X)) {
      if ((total_arg_count_807X < (*((unsigned char *) ((((char *) (-3 + code_806X))) + 2))))) {
        arg0K0 = 4;
        arg0K1 = stack_arg_count_801X;
        arg0K2 = list_args_802X;
        arg0K3 = list_arg_count_803X;
        goto L17816;}
      else {
        arg0K0 = 3;
        goto L17041;}}
    else {
      if ((63 < protocol_971X)) {
        if ((65 == protocol_971X)) {
          wants_stack_args_975X = ((((*((unsigned char *) ((((char *) (-3 + code_806X))) + 2))))<<8)) + (*((unsigned char *) ((((char *) (-3 + code_806X))) + 3)));
          if ((total_arg_count_807X < wants_stack_args_975X)) {
            arg0K0 = 4;
            arg0K1 = stack_arg_count_801X;
            arg0K2 = list_args_802X;
            arg0K3 = list_arg_count_803X;
            goto L17816;}
          else {
            merged_arg0K0 = wants_stack_args_975X;
            merged_arg0K1 = stack_arg_count_801X;
            merged_arg0K2 = list_args_802X;
            merged_arg0K3 = list_arg_count_803X;
            rest_list_setup_return_tag = 1;
            goto rest_list_setup;
           rest_list_setup_return_1:
            arg0K0 = 4;
            arg0K1 = (1 + wants_stack_args_975X);
            goto L17016;}}
        else {
          if ((64 == protocol_971X)) {
            if (((((((*((unsigned char *) ((((char *) (-3 + code_806X))) + 2))))<<8)) + (*((unsigned char *) ((((char *) (-3 + code_806X))) + 3)))) == total_arg_count_807X)) {
              merged_arg0K0 = list_args_802X;
              merged_arg0K1 = list_arg_count_803X;
              push_list_return_tag = 1;
              goto push_list;
             push_list_return_1:
              arg0K0 = 4;
              arg0K1 = total_arg_count_807X;
              goto L17016;}
            else {
              arg0K0 = 4;
              arg0K1 = stack_arg_count_801X;
              arg0K2 = list_args_802X;
              arg0K3 = list_arg_count_803X;
              goto L17816;}}
          else {
            if ((66 == protocol_971X)) {
              length_976X = (long)(((unsigned long)(*((long *) ((((char *) (-3 + code_806X))) + -4))))>>8);
              index_977X = -2 + length_976X;
              arg0K0 = (*((unsigned char *) ((((char *) (-3 + code_806X))) + (-3 + length_976X))));
              arg0K1 = (((((*((unsigned char *) ((((char *) (-3 + code_806X))) + index_977X))))<<8)) + (*((unsigned char *) ((((char *) (-3 + code_806X))) + (1 + index_977X)))));
              goto L17012;}
            else {
              ps_error("unknown protocol", 1, protocol_971X);
              arg0K0 = 4;
              arg0K1 = stack_arg_count_801X;
              arg0K2 = list_args_802X;
              arg0K3 = list_arg_count_803X;
              goto L17816;}}}}
      else {
        if ((protocol_971X == total_arg_count_807X)) {
          merged_arg0K0 = list_args_802X;
          merged_arg0K1 = list_arg_count_803X;
          push_list_return_tag = 2;
          goto push_list;
         push_list_return_2:
          arg0K0 = 2;
          arg0K1 = total_arg_count_807X;
          goto L17016;}
        else {
          arg0K0 = 4;
          arg0K1 = stack_arg_count_801X;
          arg0K2 = list_args_802X;
          arg0K3 = list_arg_count_803X;
          goto L17816;}}}}}
 L21102: {
  cont_978X = arg0K0;
  if ((3 == (3 & cont_978X))) {
    if ((10 == (31 & ((((*((long *) ((((char *) (-3 + cont_978X))) + -4))))>>2))))) {
      next_op_979X = *((unsigned char *) ((((char *) (-3 + (*((long *) (((char *) (-3 + (*((long *) ((((char *) (-3 + cont_978X))) + 8))))))))))) + ((((*((long *) ((((char *) (-3 + cont_978X))) + 4))))>>2))));
      if ((34 == next_op_979X)) {
        pop_continuationB_return_tag = 2;
        goto pop_continuationB;
       pop_continuationB_return_2:
        arg1K0 = (Scode_pointerS);
        goto L19093;}
      else {
        if ((30 == next_op_979X)) {
          next_980X = *((long *) (((char *) (-3 + (ScontS)))));
          if (((ScontS) == (Sbottom_of_stackS))) {
            *((long *) (((char *) (-3 + (ScontS))))) = (*((long *) (((char *) (-3 + next_980X)))));
            goto L19937;}
          else {
            ScontS = next_980X;
            goto L19937;}}
        else {
          merged_arg0K0 = list_args_819X;
          merged_arg0K1 = stack_nargs_818X;
          pop_args_GlistS_return_tag = 3;
          goto pop_args_GlistS;
         pop_args_GlistS_return_3:
          args_981X = pop_args_GlistS0_return_value;
          push_exception_continuationB(4, 0);
          *((long *) (SstackS)) = 1;
          SstackS = ((SstackS) + -4);
          *((long *) (SstackS)) = args_981X;
          SstackS = ((SstackS) + -4);
          arg0K0 = 2;
          goto L17320;}}}
    else {
      goto L21108;}}
  else {
    goto L21108;}}
 L19587: {
  v_982X = arg0K0;
  merged_arg0K0 = v_982X;
  copy_stack_into_heap_return_tag = 1;
  goto copy_stack_into_heap;
 copy_stack_into_heap_return_1:
  if (((SstackS) < (Sstack_limitS))) {
    ps_error("Couldn't get default procedure space (how can this happen?)", 0);
    goto L19472;}
  else {
    goto L19472;}}
 L19481: {
  arg0K0 = (*((unsigned char *) ((Scode_pointerS) + 3)));
  goto L17549;}
 L19484: {
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L19642: {
  v_983X = arg0K0;
  merged_arg0K0 = v_983X;
  copy_stack_into_heap_return_tag = 2;
  goto copy_stack_into_heap;
 copy_stack_into_heap_return_2:
  if ((space_453X < (64 + (((((SstackS) - (Sstack_limitS)))>>2))))) {
    goto L19472;}
  else {
    ps_error("VM's stack is too small (how can this happen?)", 0);
    goto L19472;}}
 L11268: {
  if ((3 == (3 & x_473X))) {
    if ((19 == (31 & ((((*((long *) ((((char *) (-3 + x_473X))) + -4))))>>2))))) {
      arg0K0 = 5;
      goto L39127;}
    else {
      goto L11274;}}
  else {
    goto L11274;}}
 L29207: {
  SvalS = 1;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L29348: {
  if ((3 == (3 & n_475X))) {
    if ((19 == (31 & ((((*((long *) ((((char *) (-3 + n_475X))) + -4))))>>2))))) {
      goto L29359;}
    else {
      goto L29360;}}
  else {
    goto L29360;}}
 L29547: {
  if ((3 == (3 & n_476X))) {
    if ((19 == (31 & ((((*((long *) ((((char *) (-3 + n_476X))) + -4))))>>2))))) {
      goto L29558;}
    else {
      goto L29559;}}
  else {
    goto L29559;}}
 L29746: {
  if ((3 == (3 & n_477X))) {
    if ((19 == (31 & ((((*((long *) ((((char *) (-3 + n_477X))) + -4))))>>2))))) {
      goto L29757;}
    else {
      goto L29758;}}
  else {
    goto L29758;}}
 L7002: {
  b_984X = arg0K0;
  lo_a_985X = 65535 & a_832X;
  lo_b_986X = 65535 & b_984X;
  hi_a_987X = 65535 & (((a_832X)>>16));
  hi_b_988X = 65535 & (((b_984X)>>16));
  lo_c_989X = SMALL_MULTIPLY(lo_a_985X, lo_b_986X);
  v_990X = SMALL_MULTIPLY(lo_b_986X, hi_a_987X);
  v_991X = SMALL_MULTIPLY(lo_a_985X, hi_b_988X);
  mid_c_992X = v_991X + v_990X;
  c_993X = lo_c_989X + (((mid_c_992X)<<16));
  if ((0 < hi_a_987X)) {
    if ((0 < hi_b_988X)) {
      push_exception_continuationB(5, 1);
      *((long *) (SstackS)) = arg2_484X;
      SstackS = ((SstackS) + -4);
      *((long *) (SstackS)) = x_485X;
      SstackS = ((SstackS) + -4);
      arg0K0 = 2;
      goto L17320;}
    else {
      goto L7044;}}
  else {
    goto L7044;}}
 L7283: {
  b_994X = arg0K0;
  c_995X = a_833X / b_994X;
  x_996X = 0 == (a_833X % b_994X);
  if (x_996X) {
    if ((a_493X < 0)) {
      if ((b_494X < 0)) {
        goto L7338;}
      else {
        goto L7337;}}
    else {
      if ((b_494X < 0)) {
        goto L7337;}
      else {
        goto L7338;}}}
  else {
    push_exception_continuationB(5, 1);
    *((long *) (SstackS)) = arg2_491X;
    SstackS = ((SstackS) + -4);
    *((long *) (SstackS)) = x_492X;
    SstackS = ((SstackS) + -4);
    arg0K0 = 2;
    goto L17320;}}
 L7460: {
  b_997X = arg0K0;
  c_998X = a_839X / b_997X;
  if ((a_507X < 0)) {
    if ((b_508X < 0)) {
      goto L7506;}
    else {
      goto L7505;}}
  else {
    if ((b_508X < 0)) {
      goto L7505;}
    else {
      goto L7506;}}}
 L26012: {
  b_999X = arg0K0;
  c_1000X = a_840X % b_999X;
  if ((a_511X < 0)) {
    arg0K0 = (0 - c_1000X);
    goto L26016;}
  else {
    arg0K0 = c_1000X;
    goto L26016;}}
 L22000: {
  SvalS = new_851X;
  Scode_pointerS = ((Scode_pointerS) + 3);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L21984: {
  i_1001X = arg0K0;
  if ((i_1001X < 0)) {
    goto L22000;}
  else {
    SstackS = ((SstackS) + 4);
    *((long *) ((((char *) (-3 + new_851X))) + (((i_1001X)<<2)))) = (*((long *) (SstackS)));
    arg0K0 = (-1 + i_1001X);
    goto L21984;}}
 L22169: {
  i_1002X = arg0K0;
  if ((i_1002X < 0)) {
    arg0K0 = stack_nargs_856X;
    arg0K1 = rest_list_857X;
    goto L22187;}
  else {
    SstackS = ((SstackS) + 4);
    *((long *) ((((char *) (-3 + new_855X))) + (((i_1002X)<<2)))) = (*((long *) (SstackS)));
    arg0K0 = (-1 + i_1002X);
    goto L22169;}}
 L22692: {
  i_1003X = arg0K0;
  if ((i_1003X < 0)) {
    SvalS = value_862X;
    Scode_pointerS = ((Scode_pointerS) + 2);
    arg1K0 = (Scode_pointerS);
    goto L19093;}
  else {
    addr_1004X = (((char *) (-3 + value_862X))) + (((i_1003X)<<2));
    S48_WRITE_BARRIER(value_862X, addr_1004X, init_859X);
    *((long *) addr_1004X) = init_859X;
    arg0K0 = (-1 + i_1003X);
    goto L22692;}}
 L26787: {
  i_1005X = arg0K0;
  if ((i_1005X < 0)) {
    SvalS = vector_864X;
    Scode_pointerS = ((Scode_pointerS) + 1);
    arg1K0 = (Scode_pointerS);
    goto L19093;}
  else {
    *((unsigned char *) ((((char *) (-3 + vector_864X))) + i_1005X)) = init_601X;
    arg0K0 = (-1 + i_1005X);
    goto L26787;}}
 L27021: {
  i_1006X = arg0K0;
  if ((i_1006X < 0)) {
    SvalS = string_867X;
    Scode_pointerS = ((Scode_pointerS) + 1);
    arg1K0 = (Scode_pointerS);
    goto L19093;}
  else {
    *((unsigned char *) ((((char *) (-3 + string_867X))) + i_1006X)) = (init_617X);
    arg0K0 = (-1 + i_1006X);
    goto L27021;}}
 L13975: {
  i_1007X = arg0K0;
  h_1008X = arg0K1;
  if ((i_1007X < n_871X)) {
    arg0K0 = (1 + i_1007X);
    arg0K1 = (h_1008X + (((*((unsigned char *) ((((char *) (-3 + string_870X))) + i_1007X))))));
    goto L13975;}
  else {
    index_1009X = 1023 & h_1008X;
    bucket_1010X = *((long *) ((((char *) (-3 + table_869X))) + (((index_1009X)<<2))));
    arg0K0 = bucket_1010X;
    goto L13943;}}
 L25160: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  arg0K0 = 1;
  goto L17320;}
 L30486: {
  SvalS = 13;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L34071: {
  key_1011X = arg0K0;
  SstackS = ((SstackS) + 4);
  arg2_1012X = *((long *) (SstackS));
  if ((0 == (3 & (SvalS)))) {
    mode_1013X = (((SvalS))>>2);
    if ((1 == mode_1013X)) {
      goto L27449;}
    else {
      if ((2 == mode_1013X)) {
        goto L27449;}
      else {
        if ((3 == mode_1013X)) {
          goto L27449;}
        else {
          if ((4 == mode_1013X)) {
            goto L27449;}
          else {
            push_exception_continuationB(5, 1);
            *((long *) (SstackS)) = arg2_1012X;
            SstackS = ((SstackS) + -4);
            *((long *) (SstackS)) = (((mode_1013X)<<2));
            SstackS = ((SstackS) + -4);
            arg0K0 = 2;
            goto L17320;}}}}}
  else {
    push_exception_continuationB(5, 1);
    *((long *) (SstackS)) = arg2_1012X;
    SstackS = ((SstackS) + -4);
    *((long *) (SstackS)) = (SvalS);
    SstackS = ((SstackS) + -4);
    arg0K0 = 2;
    goto L17320;}}
 L35506: {
  key_1014X = arg0K0;
  obj_1015X = SvalS;
  if ((3 == (3 & obj_1015X))) {
    if ((6 == (31 & ((((*((long *) ((((char *) (-3 + obj_1015X))) + -4))))>>2))))) {
      channel_1016X = SvalS;
      if ((0 == (*((long *) (((char *) (-3 + channel_1016X))))))) {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = channel_1016X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 1;
        goto L17320;}
      else {
        status_1017X = close_channelB(channel_1016X);
        if ((status_1017X == NO_ERRORS)) {
          SvalS = 13;
          Scode_pointerS = ((Scode_pointerS) + 1);
          arg1K0 = (Scode_pointerS);
          goto L19093;}
        else {
          push_exception_continuationB(25, 1);
          *((long *) (SstackS)) = (((status_1017X)<<2));
          SstackS = ((SstackS) + -4);
          *((long *) (SstackS)) = channel_1016X;
          SstackS = ((SstackS) + -4);
          merged_arg0K0 = status_1017X;
          merged_arg0K1 = key_1014X;
          get_error_string_return_tag = 0;
          goto get_error_string;
         get_error_string_return_0:
          x_1018X = get_error_string0_return_value;
          *((long *) (SstackS)) = x_1018X;
          SstackS = ((SstackS) + -4);
          arg0K0 = 3;
          goto L17320;}}}
    else {
      goto L35514;}}
  else {
    goto L35514;}}
 L38360: {
  key_1019X = arg0K0;
  SstackS = ((SstackS) + 4);
  arg2_1020X = *((long *) (SstackS));
  SstackS = ((SstackS) + 4);
  arg3_1021X = *((long *) (SstackS));
  SstackS = ((SstackS) + 4);
  arg4_1022X = *((long *) (SstackS));
  SstackS = ((SstackS) + 4);
  arg5_1023X = *((long *) (SstackS));
  if ((0 == (3 & (arg4_1022X | arg3_1021X)))) {
    if ((1 == arg2_1020X)) {
      goto L38392;}
    else {
      if ((5 == arg2_1020X)) {
        goto L38392;}
      else {
        goto L38412;}}}
  else {
    goto L38412;}}
 L38632: {
  key_1024X = arg0K0;
  SstackS = ((SstackS) + 4);
  arg2_1025X = *((long *) (SstackS));
  SstackS = ((SstackS) + 4);
  arg3_1026X = *((long *) (SstackS));
  SstackS = ((SstackS) + 4);
  arg4_1027X = *((long *) (SstackS));
  if ((0 == (3 & (arg3_1026X | arg2_1025X)))) {
    obj_1028X = SvalS;
    if ((3 == (3 & obj_1028X))) {
      if ((6 == (31 & ((((*((long *) ((((char *) (-3 + obj_1028X))) + -4))))>>2))))) {
        start_1029X = ((arg3_1026X)>>2);
        count_1030X = ((arg2_1025X)>>2);
        channel_1031X = SvalS;
        v_1032X = 8 == (*((long *) (((char *) (-3 + channel_1031X)))));
        if (v_1032X) {
          if ((3 == (3 & arg4_1027X))) {
            if ((17 == (31 & ((((*((long *) ((((char *) (-3 + arg4_1027X))) + -4))))>>2))))) {
              goto L37934;}
            else {
              goto L37926;}}
          else {
            goto L37926;}}
        else {
          arg0K0 = 5;
          goto L37809;}}
      else {
        goto L38674;}}
    else {
      goto L38674;}}
  else {
    goto L38674;}}
 L28633: {
  inputP_1033X = arg2K0;
  x_1034X = ps_add_pending_fd(((((*((long *) ((((char *) (-3 + arg2_653X))) + 8))))>>2)), inputP_1033X);
  if (x_1034X) {
    arg0K0 = 5;
    goto L28669;}
  else {
    arg0K0 = 1;
    goto L28669;}}
 L34991: {
  key_1035X = arg0K0;
  obj_1036X = SvalS;
  if ((3 == (3 & obj_1036X))) {
    if ((6 == (31 & ((((*((long *) ((((char *) (-3 + obj_1036X))) + -4))))>>2))))) {
      channel_1037X = SvalS;
      if ((0 == (*((long *) (((char *) (-3 + channel_1037X))))))) {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = channel_1037X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 1;
        goto L17320;}
      else {
        readyP_1038X = ps_check_fd(((((*((long *) ((((char *) (-3 + channel_1037X))) + 8))))>>2)), (4 == (*((long *) (((char *) (-3 + channel_1037X)))))), &status_1039X);
        if ((status_1039X == NO_ERRORS)) {
          if (readyP_1038X) {
            arg0K0 = 5;
            goto L30072;}
          else {
            arg0K0 = 1;
            goto L30072;}}
        else {
          push_exception_continuationB(25, 1);
          *((long *) (SstackS)) = (((status_1039X)<<2));
          SstackS = ((SstackS) + -4);
          *((long *) (SstackS)) = channel_1037X;
          SstackS = ((SstackS) + -4);
          merged_arg0K0 = status_1039X;
          merged_arg0K1 = key_1035X;
          get_error_string_return_tag = 1;
          goto get_error_string;
         get_error_string_return_1:
          x_1040X = get_error_string0_return_value;
          *((long *) (SstackS)) = x_1040X;
          SstackS = ((SstackS) + -4);
          arg0K0 = 3;
          goto L17320;}}}
    else {
      goto L34999;}}
  else {
    goto L34999;}}
 L8517: {
  head_1041X = arg0K0;
  if ((1 == head_1041X)) {
    addr_1042X = (((char *) (-3 + channel_659X))) + 16;
    S48_WRITE_BARRIER(channel_659X, addr_1042X, 1);
    *((long *) addr_1042X) = 1;
    n_1043X = ps_abort_fd_op(((((*((long *) ((((char *) (-3 + channel_659X))) + 8))))>>2)));
    arg0K0 = (((n_1043X)<<2));
    goto L34176;}
  else {
    if ((channel_659X == head_1041X)) {
      if (inputP_886X) {
        channel_1044X = Spending_input_channels_headS;
        next_1045X = *((long *) ((((char *) (-3 + channel_1044X))) + 12));
        Spending_input_channels_headS = next_1045X;
        addr_1046X = (((char *) (-3 + channel_1044X))) + 12;
        S48_WRITE_BARRIER(channel_1044X, addr_1046X, 1);
        *((long *) addr_1046X) = 1;
        if ((1 == next_1045X)) {
          Spending_input_channels_tailS = 1;
          goto L8539;}
        else {
          goto L8539;}}
      else {
        channel_1047X = Spending_output_channels_headS;
        next_1048X = *((long *) ((((char *) (-3 + channel_1047X))) + 12));
        Spending_output_channels_headS = next_1048X;
        addr_1049X = (((char *) (-3 + channel_1047X))) + 12;
        S48_WRITE_BARRIER(channel_1047X, addr_1049X, 1);
        *((long *) addr_1049X) = 1;
        if ((1 == next_1048X)) {
          Spending_output_channels_tailS = 1;
          goto L8550;}
        else {
          goto L8550;}}}
    else {
      arg0K0 = (*((long *) ((((char *) (-3 + head_1041X))) + 12)));
      arg0K1 = head_1041X;
      goto L8566;}}}
 L15432: {
  i_1050X = arg0K0;
  res_1051X = arg0K1;
  if ((-1 == i_1050X)) {
    SvalS = res_1051X;
    Scode_pointerS = ((Scode_pointerS) + 1);
    arg1K0 = (Scode_pointerS);
    goto L19093;}
  else {
    channel_1052X = *((Svm_channelsS) + i_1050X);
    if ((3 == (3 & channel_1052X))) {
      if ((6 == (31 & ((((*((long *) ((((char *) (-3 + channel_1052X))) + -4))))>>2))))) {
        addr_1053X = ALLOCATE_SPACE(0, 12);
        *((long *) addr_1053X) = 2050;
        x_1054X = 3 + (((long) (addr_1053X + 4)));
        *((long *) (((char *) (-3 + x_1054X)))) = channel_1052X;
        *((long *) ((((char *) (-3 + x_1054X))) + 4)) = res_1051X;
        arg0K0 = x_1054X;
        goto L15446;}
      else {
        arg0K0 = res_1051X;
        goto L15446;}}
    else {
      arg0K0 = res_1051X;
      goto L15446;}}}
 L35579: {
  key_1055X = arg0K0;
  SstackS = ((SstackS) + 4);
  arg2_1056X = *((long *) (SstackS));
  SstackS = ((SstackS) + 4);
  arg3_1057X = *((long *) (SstackS));
  if ((3 == (3 & arg3_1057X))) {
    if ((17 == (31 & ((((*((long *) ((((char *) (-3 + arg3_1057X))) + -4))))>>2))))) {
      obj_1058X = SvalS;
      if ((3 == (3 & obj_1058X))) {
        if ((17 == (31 & ((((*((long *) ((((char *) (-3 + obj_1058X))) + -4))))>>2))))) {
          comment_string_1059X = SvalS;
          x_1060X = s48_image_writing_okayP();
          if (x_1060X) {
            port_1061X = ps_open_output_file((((char *)(((char *) (-3 + arg3_1057X))))), &status_1062X);
            if ((status_1062X == NO_ERRORS)) {
              status_1063X = ps_write_string((((char *)(((char *) (-3 + comment_string_1059X))))), port_1061X);
              if ((status_1063X == NO_ERRORS)) {
                v_1064X = s48_newspaceLoldspaceP();
                if (v_1064X) {
                  merged_arg0K0 = arg2_1056X;
                  collect_saving_temp_return_tag = 1;
                  goto collect_saving_temp;
                 collect_saving_temp_return_1:
                  v_1065X = collect_saving_temp0_return_value;
                  arg0K0 = v_1065X;
                  goto L31214;}
                else {
                  arg0K0 = arg2_1056X;
                  goto L31214;}}
              else {
                status_1066X = ps_close(port_1061X);
                if ((status_1066X == NO_ERRORS)) {
                  arg0K0 = 25;
                  arg0K1 = status_1063X;
                  goto L31133;}
                else {
                  ps_write_string("Unable to close image file", (stderr));
                  { long ignoreXX;
                  PS_WRITE_CHAR(10, (stderr), ignoreXX) }
                  arg0K0 = 25;
                  arg0K1 = status_1063X;
                  goto L31133;}}}
            else {
              arg0K0 = 10;
              arg0K1 = status_1062X;
              goto L31133;}}
          else {
            push_exception_continuationB(15, 1);
            *((long *) (SstackS)) = arg3_1057X;
            SstackS = ((SstackS) + -4);
            *((long *) (SstackS)) = arg2_1056X;
            SstackS = ((SstackS) + -4);
            *((long *) (SstackS)) = comment_string_1059X;
            SstackS = ((SstackS) + -4);
            arg0K0 = 3;
            goto L17320;}}
        else {
          goto L35611;}}
      else {
        goto L35611;}}
    else {
      goto L35611;}}
  else {
    goto L35611;}}
 L31537: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = stob_891X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = proc_892X;
  SstackS = ((SstackS) + -4);
  arg0K0 = 2;
  goto L17320;}
 L35157: {
  minutesP_1067X = arg2K0;
  if ((s48_Spending_interruptPS)) {
    if ((s48_Spending_eventsPS)) {
      s48_Spending_eventsPS = 0;
      check_events_return_tag = 2;
      goto check_events;
     check_events_return_2:
      temp_1068X = check_events0_return_value;
      if (temp_1068X) {
        goto L35183;}
      else {
        goto L35188;}}
    else {
      goto L35183;}}
  else {
    goto L35188;}}
 L23513: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = proc_909X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = name_910X;
  SstackS = ((SstackS) + -4);
  arg0K0 = 2;
  goto L17320;}
 L34342: {
  key_1069X = arg0K0;
  SstackS = ((SstackS) + 4);
  arg2_1070X = *((long *) (SstackS));
  if ((3 == (3 & arg2_1070X))) {
    if ((17 == (31 & ((((*((long *) ((((char *) (-3 + arg2_1070X))) + -4))))>>2))))) {
      x_1071X = SvalS;
      if ((1 == x_1071X)) {
        goto L34359;}
      else {
        if ((5 == x_1071X)) {
          goto L34359;}
        else {
          goto L34364;}}}
    else {
      goto L34364;}}
  else {
    goto L34364;}}
 L24610: {
  table_1072X = arg0K0;
  n_1073X = -1 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + arg2_703X))) + -4))))>>8));
  arg0K0 = 0;
  arg0K1 = 0;
  goto L13522;}
 L28054: {
  len_1074X = 1 + n_920X;
  addr_1075X = ALLOCATE_SPACE(17, (4 + len_1074X));
  *((long *) addr_1075X) = (70 + (((len_1074X)<<8)));
  string_1076X = 3 + (((long) (addr_1075X + 4)));
  *((unsigned char *) ((((char *) (-3 + string_1076X))) + n_920X)) = 0;
  arg0K0 = arg2_919X;
  arg0K1 = (-1 + n_920X);
  goto L28031;}
 L28006: {
  if ((25 == arg2_919X)) {
    goto L28054;}
  else {
    push_exception_continuationB(5, 1);
    *((long *) (SstackS)) = arg2_919X;
    SstackS = ((SstackS) + -4);
    *((long *) (SstackS)) = (((n_920X)<<2));
    SstackS = ((SstackS) + -4);
    arg0K0 = 2;
    goto L17320;}}
 L31942: {
  if ((from_index_744X < 0)) {
    goto L31996;}
  else {
    if ((to_index_745X < 0)) {
      goto L31996;}
    else {
      if ((count_746X < 0)) {
        goto L31996;}
      else {
        if ((3 == (3 & arg5_743X))) {
          if ((17 == (31 & ((((*((long *) ((((char *) (-3 + arg5_743X))) + -4))))>>2))))) {
            arg0K0 = (-1 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + arg5_743X))) + -4))))>>8)));
            goto L31963;}
          else {
            goto L31959;}}
        else {
          goto L31959;}}}}}
 L31937: {
  if ((3 == (3 & arg5_743X))) {
    if ((18 == (31 & ((((*((long *) ((((char *) (-3 + arg5_743X))) + -4))))>>2))))) {
      goto L31942;}
    else {
      goto L31996;}}
  else {
    goto L31996;}}
 L31996: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = arg5_743X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((from_index_744X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = arg3_741X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((to_index_745X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((count_746X)<<2));
  SstackS = ((SstackS) + -4);
  arg0K0 = 5;
  goto L17320;}
 L23769: {
  push_exception_continuationB(5, 2);
  *((long *) (SstackS)) = port_926X;
  SstackS = ((SstackS) + -4);
  arg0K0 = 1;
  goto L17320;}
 L23749: {
  push_exception_continuationB(14, 2);
  *((long *) (SstackS)) = port_926X;
  SstackS = ((SstackS) + -4);
  arg0K0 = 1;
  goto L17320;}
 L23989: {
  push_exception_continuationB(5, 2);
  *((long *) (SstackS)) = port_934X;
  SstackS = ((SstackS) + -4);
  arg0K0 = 1;
  goto L17320;}
 L23969: {
  push_exception_continuationB(14, 2);
  *((long *) (SstackS)) = port_934X;
  SstackS = ((SstackS) + -4);
  arg0K0 = 1;
  goto L17320;}
 L24203: {
  push_exception_continuationB(5, 2);
  *((long *) (SstackS)) = Kchar_940X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = port_941X;
  SstackS = ((SstackS) + -4);
  arg0K0 = 2;
  goto L17320;}
 L24185: {
  push_exception_continuationB(14, 2);
  *((long *) (SstackS)) = Kchar_940X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = port_941X;
  SstackS = ((SstackS) + -4);
  arg0K0 = 2;
  goto L17320;}
 L30394: {
  arg0K0 = (*((long *) ((((char *) (-3 + stuff_948X))) + 4)));
  goto L30388;}
 L12292: {
  if ((3 == (3 & thing_949X))) {
    if ((17 == (31 & ((((*((long *) ((((char *) (-3 + thing_949X))) + -4))))>>2))))) {
      arg3K0 = (((char *)(((char *) (-3 + thing_949X)))));
      goto L12334;}
    else {
      goto L12300;}}
  else {
    goto L12300;}}
 L30379: {
  { long ignoreXX;
  PS_WRITE_CHAR(10, out_755X, ignoreXX) }
  SvalS = 13;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L17396: {
  StemplateS = (SvalS);
  Slosing_opcodeS = opcode_757X;
  arg0K0 = (2 + nargs_756X);
  goto L16721;}
 L17466: {
  merged_arg3K0 = "exception handler is not a closure";
  loseD0_return_tag = 1;
  goto loseD0;
 loseD0_return_1:
  goto L17396;}
 L21630: {
  key_1077X = arg0K0;
  p_1078X = SenvS;
  if ((3 == (3 & p_1078X))) {
    if ((p_1078X < (((long) (Sstack_beginS))))) {
      goto L21725;}
    else {
      if (((((long) (Sstack_endS))) < p_1078X)) {
        goto L21725;}
      else {
        merged_arg0K0 = (SenvS);
        merged_arg0K1 = (ScontS);
        merged_arg0K2 = key_1077X;
        merged_arg0K3 = 0;
        save_env_in_heap_return_tag = 0;
        goto save_env_in_heap;
       save_env_in_heap_return_0:
        v_1079X = save_env_in_heap0_return_value;
        SenvS = v_1079X;
        goto L21725;}}}
  else {
    goto L21725;}}
 L21641: {
  env_1080X = arg0K0;
  a_1081X = *((long *) ((((char *) (-3 + (StemplateS)))) + ((((((((*((unsigned char *) ((Scode_pointerS) + 1))))<<8)) + (*((unsigned char *) ((Scode_pointerS) + 2)))))<<2))));
  addr_1082X = ALLOCATE_SPACE(3, 12);
  *((long *) addr_1082X) = 2062;
  x_1083X = 3 + (((long) (addr_1082X + 4)));
  *((long *) (((char *) (-3 + x_1083X)))) = a_1081X;
  *((long *) ((((char *) (-3 + x_1083X))) + 4)) = env_1080X;
  if ((3 == (3 & x_1083X))) {
    if ((0 == (128 & (*((long *) ((((char *) (-3 + x_1083X))) + -4)))))) {
      *((long *) ((((char *) (-3 + x_1083X))) + -4)) = (128 | (*((long *) ((((char *) (-3 + x_1083X))) + -4))));
      arg0K0 = x_1083X;
      goto L21649;}
    else {
      arg0K0 = x_1083X;
      goto L21649;}}
  else {
    arg0K0 = x_1083X;
    goto L21649;}}
 L21452: {
  i_1084X = arg0K0;
  offset_1085X = arg0K1;
  env_1086X = arg0K2;
  if ((i_1084X == total_count_410X)) {
    SvalS = new_env_779X;
    Scode_pointerS = ((Scode_pointerS) + (1 + offset_1085X));
    arg1K0 = (Scode_pointerS);
    goto L19093;}
  else {
    back_1087X = *((unsigned char *) ((Scode_pointerS) + (1 + offset_1085X)));
    arg0K0 = env_1086X;
    arg0K1 = back_1087X;
    goto L21548;}}
 L21348: {
  value_1088X = arg0K0;
  SvalS = value_1088X;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L17654: {
  i_1089X = arg0K0;
  m_1090X = arg0K1;
  if ((0 == (n_963X & m_1090X))) {
    arg0K0 = (1 + i_1089X);
    arg0K1 = (((m_1090X)<<1));
    goto L17654;}
  else {
    Spending_interruptsS = ((Spending_interruptsS) & (~ m_1090X));
    if ((i_1089X == 0)) {
      *((long *) (SstackS)) = (Sinterrupted_templateS);
      SstackS = ((SstackS) + -4);
      Sinterrupted_templateS = 1;
      *((long *) (SstackS)) = ((((Senabled_interruptsS))<<2));
      SstackS = ((SstackS) + -4);
      arg0K0 = 2;
      goto L17573;}
    else {
      if ((i_1089X == 2)) {
        *((long *) (SstackS)) = (Sfinalize_theseS);
        SstackS = ((SstackS) + -4);
        Sfinalize_theseS = 25;
        *((long *) (SstackS)) = ((((Senabled_interruptsS))<<2));
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17573;}
      else {
        if ((i_1089X == 3)) {
          channel_1091X = Spending_input_channels_headS;
          next_1092X = *((long *) ((((char *) (-3 + channel_1091X))) + 12));
          Spending_input_channels_headS = next_1092X;
          addr_1093X = (((char *) (-3 + channel_1091X))) + 12;
          S48_WRITE_BARRIER(channel_1091X, addr_1093X, 1);
          *((long *) addr_1093X) = 1;
          if ((1 == next_1092X)) {
            Spending_input_channels_tailS = 1;
            arg0K0 = channel_1091X;
            goto L8820;}
          else {
            arg0K0 = channel_1091X;
            goto L8820;}}
        else {
          if ((i_1089X == 4)) {
            channel_1094X = Spending_output_channels_headS;
            next_1095X = *((long *) ((((char *) (-3 + channel_1094X))) + 12));
            Spending_output_channels_headS = next_1095X;
            addr_1096X = (((char *) (-3 + channel_1094X))) + 12;
            S48_WRITE_BARRIER(channel_1094X, addr_1096X, 1);
            *((long *) addr_1096X) = 1;
            if ((1 == next_1095X)) {
              Spending_output_channels_tailS = 1;
              arg0K0 = channel_1094X;
              goto L8848;}
            else {
              arg0K0 = channel_1094X;
              goto L8848;}}
          else {
            if ((i_1089X == 5)) {
              *((long *) (SstackS)) = (*((long *) (((char *) (-3 + (Sos_signal_listS))))));
              SstackS = ((SstackS) + -4);
              Sos_signal_listS = (*((long *) ((((char *) (-3 + (Sos_signal_listS)))) + 4)));
              x_1097X = Sos_signal_listS;
              if ((25 == x_1097X)) {
                goto L8895;}
              else {
                Spending_interruptsS = (32 | (Spending_interruptsS));
                if ((0 == ((Spending_interruptsS) & (Senabled_interruptsS)))) {
                  s48_Spending_interruptPS = 0;
                  if ((s48_Spending_eventsPS)) {
                    s48_Spending_interruptPS = 1;
                    goto L8895;}
                  else {
                    goto L8895;}}
                else {
                  s48_Spending_interruptPS = 1;
                  goto L8895;}}}
            else {
              *((long *) (SstackS)) = ((((Senabled_interruptsS))<<2));
              SstackS = ((SstackS) + -4);
              arg0K0 = 1;
              goto L17573;}}}}}}}
 L16739: {
  skip_1098X = arg0K0;
  stack_arg_count_1099X = arg0K1;
  template_1100X = *((long *) (((char *) (-3 + (SvalS)))));
  StemplateS = template_1100X;
  Scode_pointerS = ((((char *) (-3 + (*((long *) (((char *) (-3 + template_1100X)))))))) + skip_1098X);
  SenvS = (*((long *) ((((char *) (-3 + (SvalS)))) + 4)));
  arg0K0 = stack_space_965X;
  arg0K1 = stack_arg_count_1099X;
  goto L16629;}
 L16762: {
  skip_1101X = arg0K0;
  *((long *) (SstackS)) = 25;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((stack_arg_count_788X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((stack_arg_count_788X)<<2));
  SstackS = ((SstackS) + -4);
  arg0K0 = skip_1101X;
  arg0K1 = (3 + stack_arg_count_788X);
  goto L16739;}
 L17016: {
  skip_1102X = arg0K0;
  stack_arg_count_1103X = arg0K1;
  template_1104X = *((long *) (((char *) (-3 + (SvalS)))));
  StemplateS = template_1104X;
  Scode_pointerS = ((((char *) (-3 + (*((long *) (((char *) (-3 + template_1104X)))))))) + skip_1102X);
  SenvS = (*((long *) ((((char *) (-3 + (SvalS)))) + 4)));
  arg0K0 = stack_space_972X;
  arg0K1 = stack_arg_count_1103X;
  goto L16629;}
 L17041: {
  skip_1105X = arg0K0;
  if ((total_arg_count_807X < 3)) {
    arg0K0 = total_arg_count_807X;
    goto L17049;}
  else {
    if ((2 < stack_arg_count_801X)) {
      arg0K0 = stack_arg_count_801X;
      goto L17049;}
    else {
      arg0K0 = 2;
      goto L17049;}}}
 L19937: {
  SvalS = (*((long *) ((((char *) (-3 + cont_978X))) + 16)));
  arg0K0 = stack_nargs_818X;
  arg0K1 = list_args_819X;
  arg0K2 = list_arg_count_820X;
  goto L18321;}
 L21108: {
  merged_arg0K0 = list_args_819X;
  merged_arg0K1 = stack_nargs_818X;
  pop_args_GlistS_return_tag = 4;
  goto pop_args_GlistS;
 pop_args_GlistS_return_4:
  args_1106X = pop_args_GlistS0_return_value;
  push_exception_continuationB(4, 0);
  *((long *) (SstackS)) = 1;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = args_1106X;
  SstackS = ((SstackS) + -4);
  arg0K0 = 2;
  goto L17320;}
 L11274: {
  if ((3 == (3 & x_473X))) {
    if ((11 == (31 & ((((*((long *) ((((char *) (-3 + x_473X))) + -4))))>>2))))) {
      arg0K0 = 5;
      goto L39127;}
    else {
      arg0K0 = 1;
      goto L39127;}}
  else {
    arg0K0 = 1;
    goto L39127;}}
 L29360: {
  if ((3 == (3 & n_475X))) {
    if ((11 == (31 & ((((*((long *) ((((char *) (-3 + n_475X))) + -4))))>>2))))) {
      push_exception_continuationB(5, 1);
      *((long *) (SstackS)) = n_475X;
      SstackS = ((SstackS) + -4);
      arg0K0 = 1;
      goto L17320;}
    else {
      goto L29364;}}
  else {
    goto L29364;}}
 L29559: {
  if ((3 == (3 & n_476X))) {
    if ((11 == (31 & ((((*((long *) ((((char *) (-3 + n_476X))) + -4))))>>2))))) {
      push_exception_continuationB(5, 1);
      *((long *) (SstackS)) = n_476X;
      SstackS = ((SstackS) + -4);
      arg0K0 = 1;
      goto L17320;}
    else {
      goto L29563;}}
  else {
    goto L29563;}}
 L29758: {
  if ((3 == (3 & n_477X))) {
    if ((11 == (31 & ((((*((long *) ((((char *) (-3 + n_477X))) + -4))))>>2))))) {
      push_exception_continuationB(5, 1);
      *((long *) (SstackS)) = n_477X;
      SstackS = ((SstackS) + -4);
      arg0K0 = 1;
      goto L17320;}
    else {
      goto L29762;}}
  else {
    goto L29762;}}
 L7044: {
  if ((536870911 < lo_c_989X)) {
    push_exception_continuationB(5, 1);
    *((long *) (SstackS)) = arg2_484X;
    SstackS = ((SstackS) + -4);
    *((long *) (SstackS)) = x_485X;
    SstackS = ((SstackS) + -4);
    arg0K0 = 2;
    goto L17320;}
  else {
    if ((lo_c_989X < 0)) {
      push_exception_continuationB(5, 1);
      *((long *) (SstackS)) = arg2_484X;
      SstackS = ((SstackS) + -4);
      *((long *) (SstackS)) = x_485X;
      SstackS = ((SstackS) + -4);
      arg0K0 = 2;
      goto L17320;}
    else {
      if ((8192 < mid_c_992X)) {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = arg2_484X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = x_485X;
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17320;}
      else {
        if ((a_486X < 0)) {
          if ((b_487X < 0)) {
            goto L7071;}
          else {
            goto L7078;}}
        else {
          if ((b_487X < 0)) {
            goto L7078;}
          else {
            goto L7071;}}}}}}
 L7338: {
  if ((536870911 < c_995X)) {
    push_exception_continuationB(5, 1);
    *((long *) (SstackS)) = arg2_491X;
    SstackS = ((SstackS) + -4);
    *((long *) (SstackS)) = x_492X;
    SstackS = ((SstackS) + -4);
    arg0K0 = 2;
    goto L17320;}
  else {
    SvalS = (((c_995X)<<2));
    Scode_pointerS = ((Scode_pointerS) + 1);
    arg1K0 = (Scode_pointerS);
    goto L19093;}}
 L7337: {
  SvalS = ((((0 - c_995X))<<2));
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L7506: {
  if ((536870911 < c_998X)) {
    push_exception_continuationB(5, 1);
    *((long *) (SstackS)) = arg2_505X;
    SstackS = ((SstackS) + -4);
    *((long *) (SstackS)) = x_506X;
    SstackS = ((SstackS) + -4);
    arg0K0 = 2;
    goto L17320;}
  else {
    SvalS = (((c_998X)<<2));
    Scode_pointerS = ((Scode_pointerS) + 1);
    arg1K0 = (Scode_pointerS);
    goto L19093;}}
 L7505: {
  SvalS = ((((0 - c_998X))<<2));
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L26016: {
  n_1107X = arg0K0;
  SvalS = (((n_1107X)<<2));
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L22187: {
  i_1108X = arg0K0;
  rest_list_1109X = arg0K1;
  if ((25 == rest_list_1109X)) {
    SvalS = new_855X;
    Scode_pointerS = ((Scode_pointerS) + 2);
    arg1K0 = (Scode_pointerS);
    goto L19093;}
  else {
    *((long *) ((((char *) (-3 + new_855X))) + (((i_1108X)<<2)))) = (*((long *) (((char *) (-3 + rest_list_1109X)))));
    arg0K0 = (1 + i_1108X);
    arg0K1 = (*((long *) ((((char *) (-3 + rest_list_1109X))) + 4)));
    goto L22187;}}
 L13943: {
  foo_1110X = arg0K0;
  if ((1 == foo_1110X)) {
    addr_1111X = ALLOCATE_SPACE(1, 12);
    *((long *) addr_1111X) = 2054;
    x_1112X = 3 + (((long) (addr_1111X + 4)));
    *((long *) (((char *) (-3 + x_1112X)))) = string_870X;
    *((long *) ((((char *) (-3 + x_1112X))) + 4)) = bucket_1010X;
    addr_1113X = (((char *) (-3 + table_869X))) + (((index_1009X)<<2));
    S48_WRITE_BARRIER(table_869X, addr_1113X, x_1112X);
    *((long *) addr_1113X) = x_1112X;
    arg0K0 = x_1112X;
    goto L25201;}
  else {
    s2_1114X = *((long *) (((char *) (-3 + foo_1110X))));
    len_1115X = (long)(((unsigned long)(*((long *) ((((char *) (-3 + string_870X))) + -4))))>>8);
    if ((len_1115X == ((long)(((unsigned long)(*((long *) ((((char *) (-3 + s2_1114X))) + -4))))>>8)))) {
      if (((!memcmp((void *)(((char *) (-3 + s2_1114X))), (void *)(((char *) (-3 + string_870X))),len_1115X)))) {
        arg0K0 = foo_1110X;
        goto L25201;}
      else {
        goto L13959;}}
    else {
      goto L13959;}}}
 L27449: {
  if ((0 == (3 & arg2_1012X))) {
    if (((((arg2_1012X)>>2)) < 0)) {
      push_exception_continuationB(5, 1);
      *((long *) (SstackS)) = arg2_1012X;
      SstackS = ((SstackS) + -4);
      *((long *) (SstackS)) = (((mode_1013X)<<2));
      SstackS = ((SstackS) + -4);
      arg0K0 = 2;
      goto L17320;}
    else {
      arg0K0 = (((arg2_1012X)>>2));
      goto L27269;}}
  else {
    if ((3 == (3 & arg2_1012X))) {
      if ((17 == (31 & ((((*((long *) ((((char *) (-3 + arg2_1012X))) + -4))))>>2))))) {
        if ((1 == mode_1013X)) {
          goto L27327;}
        else {
          if ((3 == mode_1013X)) {
            goto L27327;}
          else {
            v_1116X = ps_open_fd((((char *)(((char *) (-3 + arg2_1012X))))), 0, &v_1117X);
            arg0K0 = v_1116X;
            arg0K1 = v_1117X;
            goto L27340;}}}
      else {
        push_exception_continuationB(5, 1);
        *((long *) (SstackS)) = arg2_1012X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = (((mode_1013X)<<2));
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17320;}}
    else {
      push_exception_continuationB(5, 1);
      *((long *) (SstackS)) = arg2_1012X;
      SstackS = ((SstackS) + -4);
      *((long *) (SstackS)) = (((mode_1013X)<<2));
      SstackS = ((SstackS) + -4);
      arg0K0 = 2;
      goto L17320;}}}
 L35514: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  arg0K0 = 1;
  goto L17320;}
 L38392: {
  obj_1118X = SvalS;
  if ((3 == (3 & obj_1118X))) {
    if ((6 == (31 & ((((*((long *) ((((char *) (-3 + obj_1118X))) + -4))))>>2))))) {
      x_1119X = SvalS;
      if ((1 == arg2_1020X)) {
        arg2K0 = 0;
        goto L38409;}
      else {
        arg2K0 = 1;
        goto L38409;}}
    else {
      goto L38412;}}
  else {
    goto L38412;}}
 L38412: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = arg5_1023X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = arg4_1022X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = arg3_1021X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = arg2_1020X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  arg0K0 = 5;
  goto L17320;}
 L37934: {
  if ((3 == (3 & arg4_1027X))) {
    if ((17 == (31 & ((((*((long *) ((((char *) (-3 + arg4_1027X))) + -4))))>>2))))) {
      arg0K0 = (-1 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + arg4_1027X))) + -4))))>>8)));
      goto L37946;}
    else {
      goto L37942;}}
  else {
    goto L37942;}}
 L37926: {
  if ((3 == (3 & arg4_1027X))) {
    if ((18 == (31 & ((((*((long *) ((((char *) (-3 + arg4_1027X))) + -4))))>>2))))) {
      goto L37934;}
    else {
      arg0K0 = 5;
      goto L37809;}}
  else {
    arg0K0 = 5;
    goto L37809;}}
 L37809: {
  reason_1120X = arg0K0;
  push_exception_continuationB(reason_1120X, 1);
  *((long *) (SstackS)) = arg4_1027X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((start_1029X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((count_1030X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = channel_1031X;
  SstackS = ((SstackS) + -4);
  arg0K0 = 4;
  goto L17320;}
 L38674: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = arg4_1027X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = arg3_1026X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = arg2_1025X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  arg0K0 = 4;
  goto L17320;}
 L28669: {
  val_1121X = arg0K0;
  SvalS = val_1121X;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L30072: {
  val_1122X = arg0K0;
  SvalS = val_1122X;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L34999: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  arg0K0 = 1;
  goto L17320;}
 L34176: {
  val_1123X = arg0K0;
  SvalS = val_1123X;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L8539: {
  if ((1 == (Spending_input_channels_headS))) {
    Spending_interruptsS = (-9 & (Spending_interruptsS));
    goto L8560;}
  else {
    goto L8560;}}
 L8550: {
  if ((1 == (Spending_output_channels_headS))) {
    Spending_interruptsS = (-17 & (Spending_interruptsS));
    goto L8560;}
  else {
    goto L8560;}}
 L8566: {
  ch_1124X = arg0K0;
  prev_1125X = arg0K1;
  if ((1 == ch_1124X)) {
    addr_1126X = (((char *) (-3 + channel_659X))) + 16;
    S48_WRITE_BARRIER(channel_659X, addr_1126X, 1);
    *((long *) addr_1126X) = 1;
    n_1127X = ps_abort_fd_op(((((*((long *) ((((char *) (-3 + channel_659X))) + 8))))>>2)));
    arg0K0 = (((n_1127X)<<2));
    goto L34176;}
  else {
    if ((ch_1124X == channel_659X)) {
      if (inputP_886X) {
        if ((ch_1124X == (Spending_input_channels_tailS))) {
          Spending_input_channels_tailS = prev_1125X;
          goto L8597;}
        else {
          goto L8597;}}
      else {
        if ((ch_1124X == (Spending_output_channels_tailS))) {
          Spending_output_channels_tailS = prev_1125X;
          goto L8597;}
        else {
          goto L8597;}}}
    else {
      arg0K0 = (*((long *) ((((char *) (-3 + ch_1124X))) + 12)));
      arg0K1 = ch_1124X;
      goto L8566;}}}
 L15446: {
  v_1128X = arg0K0;
  arg0K0 = (-1 + i_1050X);
  arg0K1 = v_1128X;
  goto L15432;}
 L31214: {
  resume_proc_1129X = arg0K0;
  status_1130X = s48_write_image(resume_proc_1129X, port_1061X);
  if ((status_1130X == NO_ERRORS)) {
    status_1131X = ps_close(port_1061X);
    if ((status_1131X == NO_ERRORS)) {
      undumpables_1132X = s48_undumpable_records(&undumpable_count_1133X);
      if ((0 == undumpable_count_1133X)) {
        SvalS = 13;
        Scode_pointerS = ((Scode_pointerS) + 1);
        arg1K0 = (Scode_pointerS);
        goto L19093;}
      else {
        push_exception_continuationB(26, 1);
        *((long *) (SstackS)) = arg3_1057X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = resume_proc_1129X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = comment_string_1059X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = undumpables_1132X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = (((undumpable_count_1133X)<<2));
        SstackS = ((SstackS) + -4);
        arg0K0 = 5;
        goto L17320;}}
    else {
      arg0K0 = 25;
      arg0K1 = status_1131X;
      goto L31133;}}
  else {
    status_1134X = ps_close(port_1061X);
    if ((status_1134X == NO_ERRORS)) {
      arg0K0 = 25;
      arg0K1 = status_1130X;
      goto L31133;}
    else {
      ps_write_string("Unable to close image file", (stderr));
      { long ignoreXX;
      PS_WRITE_CHAR(10, (stderr), ignoreXX) }
      arg0K0 = 25;
      arg0K1 = status_1130X;
      goto L31133;}}}
 L31133: {
  reason_1135X = arg0K0;
  status_1136X = arg0K1;
  push_exception_continuationB(reason_1135X, 1);
  *((long *) (SstackS)) = (((status_1136X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = arg3_1057X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = arg2_1056X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = comment_string_1059X;
  SstackS = ((SstackS) + -4);
  merged_arg0K0 = status_1136X;
  merged_arg0K1 = key_1055X;
  get_error_string_return_tag = 2;
  goto get_error_string;
 get_error_string_return_2:
  x_1137X = get_error_string0_return_value;
  *((long *) (SstackS)) = x_1137X;
  SstackS = ((SstackS) + -4);
  arg0K0 = 5;
  goto L17320;}
 L35611: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = arg3_1057X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = arg2_1056X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  arg0K0 = 3;
  goto L17320;}
 L35183: {
  SvalS = 13;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L35188: {
  if ((0 == (Spending_interruptsS))) {
    s48_wait_for_event((((arg2_695X)>>2)), minutesP_1067X);
    goto L35183;}
  else {
    goto L35183;}}
 L34359: {
  if ((1 == (SvalS))) {
    v_1138X = Hlookup2142((Sexported_bindingsS), arg2_1070X, key_1069X);
    arg0K0 = v_1138X;
    goto L34416;}
  else {
    v_1139X = Hlookup2123((Simported_bindingsS), arg2_1070X, key_1069X);
    arg0K0 = v_1139X;
    goto L34416;}}
 L34364: {
  push_exception_continuationB(5, 1);
  *((long *) (SstackS)) = arg2_1070X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (SvalS);
  SstackS = ((SstackS) + -4);
  arg0K0 = 2;
  goto L17320;}
 L13522: {
  i_1140X = arg0K0;
  h_1141X = arg0K1;
  if ((i_1140X < n_1073X)) {
    arg0K0 = (1 + i_1140X);
    arg0K1 = (h_1141X + (((*((unsigned char *) ((((char *) (-3 + arg2_703X))) + i_1140X))))));
    goto L13522;}
  else {
    index_1142X = 1023 & h_1141X;
    bucket_1143X = *((long *) ((((char *) (-3 + table_1072X))) + (((index_1142X)<<2))));
    arg0K0 = 1;
    arg0K1 = bucket_1143X;
    goto L13486;}}
 L28031: {
  l_1144X = arg0K0;
  i_1145X = arg0K1;
  if ((i_1145X < 0)) {
    SvalS = string_1076X;
    Scode_pointerS = ((Scode_pointerS) + 1);
    arg1K0 = (Scode_pointerS);
    goto L19093;}
  else {
    *((unsigned char *) ((((char *) (-3 + string_1076X))) + i_1145X)) = ((((((*((long *) (((char *) (-3 + l_1144X))))))>>8))));
    arg0K0 = (*((long *) ((((char *) (-3 + l_1144X))) + 4)));
    arg0K1 = (-1 + i_1145X);
    goto L28031;}}
 L31963: {
  y_1146X = arg0K0;
  if ((y_1146X < (from_index_744X + count_746X))) {
    goto L31996;}
  else {
    if ((3 == (3 & arg3_741X))) {
      if ((17 == (31 & ((((*((long *) ((((char *) (-3 + arg3_741X))) + -4))))>>2))))) {
        arg0K0 = (-1 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + arg3_741X))) + -4))))>>8)));
        goto L31978;}
      else {
        goto L31974;}}
    else {
      goto L31974;}}}
 L31959: {
  arg0K0 = ((long)(((unsigned long)(*((long *) ((((char *) (-3 + arg5_743X))) + -4))))>>8));
  goto L31963;}
 L12334: {
  v_1147X = arg3K0;
  ps_write_string(v_1147X, out_755X);
  goto L30394;}
 L12300: {
  if ((3 == (3 & thing_949X))) {
    if ((1 == (31 & ((((*((long *) ((((char *) (-3 + thing_949X))) + -4))))>>2))))) {
      arg3K0 = (((char *)(((char *) (-3 + (*((long *) (((char *) (-3 + thing_949X))))))))));
      goto L12334;}
    else {
      goto L12308;}}
  else {
    goto L12308;}}
 L21725: {
  arg0K0 = (SenvS);
  goto L21635;}
 L21649: {
  value_1148X = arg0K0;
  SvalS = value_1148X;
  Scode_pointerS = ((Scode_pointerS) + 4);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L21548: {
  env_1149X = arg0K0;
  i_1150X = arg0K1;
  if ((0 == i_1150X)) {
    count_1151X = *((unsigned char *) ((Scode_pointerS) + (2 + offset_1085X)));
    arg0K0 = count_1151X;
    arg0K1 = i_1084X;
    arg0K2 = (2 + offset_1085X);
    goto L21469;}
  else {
    arg0K0 = (*((long *) (((char *) (-3 + env_1149X)))));
    arg0K1 = (-1 + i_1150X);
    goto L21548;}}
 L17573: {
  arg_count_1152X = arg0K0;
  obj_1153X = Sinterrupt_handlersS;
  if ((3 == (3 & obj_1153X))) {
    if ((2 == (31 & ((((*((long *) ((((char *) (-3 + obj_1153X))) + -4))))>>2))))) {
      goto L17587;}
    else {
      goto L17667;}}
  else {
    goto L17667;}}
 L8820: {
  channel_1154X = arg0K0;
  x_1155X = 1 == (Spending_input_channels_headS);
  if (x_1155X) {
    goto L8834;}
  else {
    PS_SHIFT_LEFT(1, i_1089X, x_1156X)
    Spending_interruptsS = ((Spending_interruptsS) | x_1156X);
    if ((0 == ((Spending_interruptsS) & (Senabled_interruptsS)))) {
      s48_Spending_interruptPS = 0;
      if ((s48_Spending_eventsPS)) {
        s48_Spending_interruptPS = 1;
        goto L8834;}
      else {
        goto L8834;}}
    else {
      s48_Spending_interruptPS = 1;
      goto L8834;}}}
 L8848: {
  channel_1157X = arg0K0;
  x_1158X = 1 == (Spending_output_channels_headS);
  if (x_1158X) {
    goto L8862;}
  else {
    PS_SHIFT_LEFT(1, i_1089X, x_1159X)
    Spending_interruptsS = ((Spending_interruptsS) | x_1159X);
    if ((0 == ((Spending_interruptsS) & (Senabled_interruptsS)))) {
      s48_Spending_interruptPS = 0;
      if ((s48_Spending_eventsPS)) {
        s48_Spending_interruptPS = 1;
        goto L8862;}
      else {
        goto L8862;}}
    else {
      s48_Spending_interruptPS = 1;
      goto L8862;}}}
 L8895: {
  *((long *) (SstackS)) = ((((Senabled_interruptsS))<<2));
  SstackS = ((SstackS) + -4);
  arg0K0 = 2;
  goto L17573;}
 L16629: {
  stack_slots_1160X = arg0K0;
  stack_arg_count_1161X = arg0K1;
  if ((stack_slots_1160X < (64 + (((((SstackS) - (Sstack_limitS)))>>2))))) {
    goto L16631;}
  else {
    space_1162X = 1 + (((((Sstack_endS) - (SstackS)))>>2));
    v_1163X = AVAILABLEp(space_1162X);
    if (v_1163X) {
      arg2K0 = 1;
      arg0K1 = 0;
      goto L16688;}
    else {
      collect_saving_temps(1, 1, &temp1_1164X);
      v_1165X = AVAILABLEp(space_1162X);
      if (v_1165X) {
        arg2K0 = 1;
        arg0K1 = 0;
        goto L16688;}
      else {
        arg2K0 = 0;
        arg0K1 = 0;
        goto L16688;}}}}
 L17049: {
  final_stack_arg_count_1166X = arg0K0;
  if ((stack_arg_count_801X < final_stack_arg_count_1166X)) {
    arg0K0 = final_stack_arg_count_1166X;
    goto L17053;}
  else {
    arg0K0 = stack_arg_count_801X;
    goto L17053;}}
 L29364: {
  SvalS = 1;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L29563: {
  SvalS = 1;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L29762: {
  SvalS = 1;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L7071: {
  if ((536870911 < c_993X)) {
    push_exception_continuationB(5, 1);
    *((long *) (SstackS)) = arg2_484X;
    SstackS = ((SstackS) + -4);
    *((long *) (SstackS)) = x_485X;
    SstackS = ((SstackS) + -4);
    arg0K0 = 2;
    goto L17320;}
  else {
    SvalS = (((c_993X)<<2));
    Scode_pointerS = ((Scode_pointerS) + 1);
    arg1K0 = (Scode_pointerS);
    goto L19093;}}
 L7078: {
  if ((536870912 < c_993X)) {
    push_exception_continuationB(5, 1);
    *((long *) (SstackS)) = arg2_484X;
    SstackS = ((SstackS) + -4);
    *((long *) (SstackS)) = x_485X;
    SstackS = ((SstackS) + -4);
    arg0K0 = 2;
    goto L17320;}
  else {
    SvalS = ((((0 - c_993X))<<2));
    Scode_pointerS = ((Scode_pointerS) + 1);
    arg1K0 = (Scode_pointerS);
    goto L19093;}}
 L25201: {
  val_1167X = arg0K0;
  SvalS = val_1167X;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L13959: {
  arg0K0 = (*((long *) ((((char *) (-3 + foo_1110X))) + 4)));
  goto L13943;}
 L27269: {
  index_1168X = arg0K0;
  channel_1169X = make_registered_channel(mode_1013X, arg2_1012X, index_1168X, key_1011X, &reason_1170X);
  if ((1 == channel_1169X)) {
    if ((3 == (3 & arg2_1012X))) {
      if ((17 == (31 & ((((*((long *) ((((char *) (-3 + arg2_1012X))) + -4))))>>2))))) {
        if ((1 == mode_1013X)) {
          goto L27384;}
        else {
          if ((3 == mode_1013X)) {
            goto L27384;}
          else {
            v_1171X = ps_close_fd(index_1168X);
            arg0K0 = v_1171X;
            goto L27379;}}}
      else {
        push_exception_continuationB(reason_1170X, 1);
        *((long *) (SstackS)) = arg2_1012X;
        SstackS = ((SstackS) + -4);
        *((long *) (SstackS)) = (((mode_1013X)<<2));
        SstackS = ((SstackS) + -4);
        arg0K0 = 2;
        goto L17320;}}
    else {
      push_exception_continuationB(reason_1170X, 1);
      *((long *) (SstackS)) = arg2_1012X;
      SstackS = ((SstackS) + -4);
      *((long *) (SstackS)) = (((mode_1013X)<<2));
      SstackS = ((SstackS) + -4);
      arg0K0 = 2;
      goto L17320;}}
  else {
    SvalS = channel_1169X;
    Scode_pointerS = ((Scode_pointerS) + 1);
    arg1K0 = (Scode_pointerS);
    goto L19093;}}
 L27327: {
  v_1172X = ps_open_fd((((char *)(((char *) (-3 + arg2_1012X))))), 1, &v_1173X);
  arg0K0 = v_1172X;
  arg0K1 = v_1173X;
  goto L27340;}
 L27340: {
  channel_1174X = arg0K0;
  status_1175X = arg0K1;
  if ((status_1175X == NO_ERRORS)) {
    arg0K0 = channel_1174X;
    goto L27269;}
  else {
    if ((status_1175X == ENOENT)) {
      push_exception_continuationB(10, 1);
      *((long *) (SstackS)) = arg2_1012X;
      SstackS = ((SstackS) + -4);
      *((long *) (SstackS)) = (((mode_1013X)<<2));
      SstackS = ((SstackS) + -4);
      arg0K0 = 2;
      goto L17320;}
    else {
      push_exception_continuationB(25, 1);
      *((long *) (SstackS)) = (((status_1175X)<<2));
      SstackS = ((SstackS) + -4);
      *((long *) (SstackS)) = arg2_1012X;
      SstackS = ((SstackS) + -4);
      *((long *) (SstackS)) = (((mode_1013X)<<2));
      SstackS = ((SstackS) + -4);
      merged_arg0K0 = status_1175X;
      merged_arg0K1 = key_1011X;
      get_error_string_return_tag = 3;
      goto get_error_string;
     get_error_string_return_3:
      x_1176X = get_error_string0_return_value;
      *((long *) (SstackS)) = x_1176X;
      SstackS = ((SstackS) + -4);
      arg0K0 = 4;
      goto L17320;}}}
 L38409: {
  waitP_1177X = arg2K0;
  start_1178X = ((arg4_1022X)>>2);
  count_1179X = ((arg3_1021X)>>2);
  if ((4 == (*((long *) (((char *) (-3 + x_1119X))))))) {
    if ((3 == (3 & arg5_1023X))) {
      if ((0 == (128 & (*((long *) ((((char *) (-3 + arg5_1023X))) + -4)))))) {
        if ((3 == (3 & arg5_1023X))) {
          if ((17 == (31 & ((((*((long *) ((((char *) (-3 + arg5_1023X))) + -4))))>>2))))) {
            goto L37519;}
          else {
            goto L37511;}}
        else {
          goto L37511;}}
      else {
        arg0K0 = 5;
        goto L37394;}}
    else {
      arg0K0 = 5;
      goto L37394;}}
  else {
    arg0K0 = 5;
    goto L37394;}}
 L37946: {
  length_1180X = arg0K0;
  if ((length_1180X < (start_1029X + count_1030X))) {
    arg0K0 = 7;
    goto L37809;}
  else {
    got_1181X = ps_write_fd(((((*((long *) ((((char *) (-3 + channel_1031X))) + 8))))>>2)), ((((char *) (-3 + arg4_1027X))) + start_1029X), count_1030X, &pendingP_1182X, &status_1183X);
    if ((status_1183X == NO_ERRORS)) {
      if (pendingP_1182X) {
        addr_1184X = (((char *) (-3 + channel_1031X))) + 16;
        S48_WRITE_BARRIER(channel_1031X, addr_1184X, 5);
        *((long *) addr_1184X) = 5;
        s48_Spending_interruptPS = 0;
        Senabled_interruptsS = 0;
        arg0K0 = 13;
        goto L37809;}
      else {
        SvalS = (((got_1181X)<<2));
        Scode_pointerS = ((Scode_pointerS) + 1);
        arg1K0 = (Scode_pointerS);
        goto L19093;}}
    else {
      push_exception_continuationB(25, 1);
      *((long *) (SstackS)) = (((status_1183X)<<2));
      SstackS = ((SstackS) + -4);
      *((long *) (SstackS)) = arg4_1027X;
      SstackS = ((SstackS) + -4);
      *((long *) (SstackS)) = (((start_1029X)<<2));
      SstackS = ((SstackS) + -4);
      *((long *) (SstackS)) = (((count_1030X)<<2));
      SstackS = ((SstackS) + -4);
      *((long *) (SstackS)) = channel_1031X;
      SstackS = ((SstackS) + -4);
      merged_arg0K0 = status_1183X;
      merged_arg0K1 = key_1024X;
      get_error_string_return_tag = 4;
      goto get_error_string;
     get_error_string_return_4:
      x_1185X = get_error_string0_return_value;
      *((long *) (SstackS)) = x_1185X;
      SstackS = ((SstackS) + -4);
      arg0K0 = 6;
      goto L17320;}}}
 L37942: {
  arg0K0 = ((long)(((unsigned long)(*((long *) ((((char *) (-3 + arg4_1027X))) + -4))))>>8));
  goto L37946;}
 L8560: {
  arg0K0 = (*((long *) ((((char *) (-3 + channel_659X))) + 16)));
  goto L34176;}
 L8597: {
  val_1186X = *((long *) ((((char *) (-3 + ch_1124X))) + 12));
  addr_1187X = (((char *) (-3 + prev_1125X))) + 12;
  S48_WRITE_BARRIER(prev_1125X, addr_1187X, val_1186X);
  *((long *) addr_1187X) = val_1186X;
  addr_1188X = (((char *) (-3 + ch_1124X))) + 12;
  S48_WRITE_BARRIER(ch_1124X, addr_1188X, 1);
  *((long *) addr_1188X) = 1;
  arg0K0 = (*((long *) ((((char *) (-3 + ch_1124X))) + 16)));
  goto L34176;}
 L34416: {
  val_1189X = arg0K0;
  SvalS = val_1189X;
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L13486: {
  previous_foo_1190X = arg0K0;
  foo_1191X = arg0K1;
  if ((1 == foo_1191X)) {
    goto L24612;}
  else {
    s2_1192X = *((long *) (((char *) (-3 + foo_1191X))));
    len_1193X = (long)(((unsigned long)(*((long *) ((((char *) (-3 + arg2_703X))) + -4))))>>8);
    if ((len_1193X == ((long)(((unsigned long)(*((long *) ((((char *) (-3 + s2_1192X))) + -4))))>>8)))) {
      if (((!memcmp((void *)(((char *) (-3 + s2_1192X))), (void *)(((char *) (-3 + arg2_703X))),len_1193X)))) {
        if ((1 == previous_foo_1190X)) {
          value_1194X = *((long *) ((((char *) (-3 + foo_1191X))) + 12));
          addr_1195X = (((char *) (-3 + table_1072X))) + (((index_1142X)<<2));
          S48_WRITE_BARRIER(table_1072X, addr_1195X, value_1194X);
          *((long *) addr_1195X) = value_1194X;
          goto L24612;}
        else {
          val_1196X = *((long *) ((((char *) (-3 + foo_1191X))) + 12));
          addr_1197X = (((char *) (-3 + previous_foo_1190X))) + 12;
          S48_WRITE_BARRIER(previous_foo_1190X, addr_1197X, val_1196X);
          *((long *) addr_1197X) = val_1196X;
          goto L24612;}}
      else {
        goto L13548;}}
    else {
      goto L13548;}}}
 L31978: {
  y_1198X = arg0K0;
  if ((y_1198X < (to_index_745X + count_746X))) {
    goto L31996;}
  else {
    memcpy((void *)((((char *) (-3 + arg3_741X))) + to_index_745X), (void *)((((char *) (-3 + arg5_743X))) + from_index_744X),count_746X);
    SvalS = 13;
    Scode_pointerS = ((Scode_pointerS) + 1);
    arg1K0 = (Scode_pointerS);
    goto L19093;}}
 L31974: {
  arg0K0 = ((long)(((unsigned long)(*((long *) ((((char *) (-3 + arg3_741X))) + -4))))>>8));
  goto L31978;}
 L12308: {
  if ((1 == thing_949X)) {
    goto L12311;}
  else {
    if ((5 == thing_949X)) {
      goto L12311;}
    else {
      if ((25 == thing_949X)) {
        arg3K0 = "()";
        goto L12334;}
      else {
        if ((3 == (3 & thing_949X))) {
          if ((0 == (31 & ((((*((long *) ((((char *) (-3 + thing_949X))) + -4))))>>2))))) {
            arg3K0 = "(...)";
            goto L12334;}
          else {
            goto L12324;}}
        else {
          goto L12324;}}}}}
 L21469: {
  count_1199X = arg0K0;
  i_1200X = arg0K1;
  offset_1201X = arg0K2;
  if ((0 == count_1199X)) {
    arg0K0 = i_1200X;
    arg0K1 = offset_1201X;
    arg0K2 = env_1149X;
    goto L21452;}
  else {
    value_1202X = *((long *) ((((char *) (-3 + env_1149X))) + ((((*((unsigned char *) ((Scode_pointerS) + (1 + offset_1201X)))))<<2))));
    addr_1203X = (((char *) (-3 + new_env_779X))) + (((i_1200X)<<2));
    S48_WRITE_BARRIER(new_env_779X, addr_1203X, value_1202X);
    *((long *) addr_1203X) = value_1202X;
    arg0K0 = (-1 + count_1199X);
    arg0K1 = (1 + i_1200X);
    arg0K2 = (1 + offset_1201X);
    goto L21469;}}
 L17587: {
  Senabled_interruptsS = 0;
  if ((0 == ((Spending_interruptsS) & (Senabled_interruptsS)))) {
    s48_Spending_interruptPS = 0;
    if ((s48_Spending_eventsPS)) {
      s48_Spending_interruptPS = 1;
      goto L17589;}
    else {
      goto L17589;}}
  else {
    s48_Spending_interruptPS = 1;
    goto L17589;}}
 L17667: {
  ps_error("interrupt handler is not a vector", 0);
  goto L17587;}
 L8834: {
  *((long *) (SstackS)) = channel_1154X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (*((long *) ((((char *) (-3 + channel_1154X))) + 16)));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = ((((Senabled_interruptsS))<<2));
  SstackS = ((SstackS) + -4);
  arg0K0 = 3;
  goto L17573;}
 L8862: {
  *((long *) (SstackS)) = channel_1157X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (*((long *) ((((char *) (-3 + channel_1157X))) + 16)));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = ((((Senabled_interruptsS))<<2));
  SstackS = ((SstackS) + -4);
  arg0K0 = 3;
  goto L17573;}
 L16631: {
  if ((s48_Spending_interruptPS)) {
    if ((s48_Spending_eventsPS)) {
      s48_Spending_eventsPS = 0;
      check_events_return_tag = 3;
      goto check_events;
     check_events_return_3:
      v_1204X = check_events0_return_value;
      if (v_1204X) {
        arg0K0 = stack_arg_count_1161X;
        goto L17549;}
      else {
        goto L16635;}}
    else {
      arg0K0 = stack_arg_count_1161X;
      goto L17549;}}
  else {
    goto L16635;}}
 L16688: {
  okayP_1205X = arg2K0;
  key_1206X = arg0K1;
  if (okayP_1205X) {
    arg0K0 = key_1206X;
    goto L16642;}
  else {
    ps_error("Scheme48 heap overflow", 0);
    arg0K0 = key_1206X;
    goto L16642;}}
 L17053: {
  v_1207X = arg0K0;
  merged_arg0K0 = v_1207X;
  merged_arg0K1 = stack_arg_count_801X;
  merged_arg0K2 = list_args_802X;
  merged_arg0K3 = list_arg_count_803X;
  rest_list_setup_return_tag = 2;
  goto rest_list_setup;
 rest_list_setup_return_2:
  *((long *) (SstackS)) = (((final_stack_arg_count_1166X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((total_arg_count_807X)<<2));
  SstackS = ((SstackS) + -4);
  arg0K0 = skip_1105X;
  arg0K1 = (3 + final_stack_arg_count_1166X);
  goto L17016;}
 L27384: {
  v_1208X = ps_close_fd(index_1168X);
  arg0K0 = v_1208X;
  goto L27379;}
 L27379: {
  status_1209X = arg0K0;
  if ((status_1209X == NO_ERRORS)) {
    push_exception_continuationB(reason_1170X, 1);
    *((long *) (SstackS)) = arg2_1012X;
    SstackS = ((SstackS) + -4);
    *((long *) (SstackS)) = (((mode_1013X)<<2));
    SstackS = ((SstackS) + -4);
    arg0K0 = 2;
    goto L17320;}
  else {
    channel_close_error(status_1209X, index_1168X, arg2_1012X);
    push_exception_continuationB(reason_1170X, 1);
    *((long *) (SstackS)) = arg2_1012X;
    SstackS = ((SstackS) + -4);
    *((long *) (SstackS)) = (((mode_1013X)<<2));
    SstackS = ((SstackS) + -4);
    arg0K0 = 2;
    goto L17320;}}
 L37519: {
  if ((3 == (3 & arg5_1023X))) {
    if ((17 == (31 & ((((*((long *) ((((char *) (-3 + arg5_1023X))) + -4))))>>2))))) {
      arg0K0 = (-1 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + arg5_1023X))) + -4))))>>8)));
      goto L37531;}
    else {
      goto L37527;}}
  else {
    goto L37527;}}
 L37511: {
  if ((3 == (3 & arg5_1023X))) {
    if ((18 == (31 & ((((*((long *) ((((char *) (-3 + arg5_1023X))) + -4))))>>2))))) {
      goto L37519;}
    else {
      arg0K0 = 5;
      goto L37394;}}
  else {
    arg0K0 = 5;
    goto L37394;}}
 L37394: {
  reason_1210X = arg0K0;
  push_exception_continuationB(reason_1210X, 1);
  *((long *) (SstackS)) = arg5_1023X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((start_1178X)<<2));
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = (((count_1179X)<<2));
  SstackS = ((SstackS) + -4);
  if (waitP_1177X) {
    arg0K0 = 5;
    goto L37409;}
  else {
    arg0K0 = 1;
    goto L37409;}}
 L24612: {
  Scode_pointerS = ((Scode_pointerS) + 1);
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L13548: {
  arg0K0 = foo_1191X;
  arg0K1 = (*((long *) ((((char *) (-3 + foo_1191X))) + 12)));
  goto L13486;}
 L12311: {
  if ((1 == thing_949X)) {
    arg3K0 = "#f";
    goto L12334;}
  else {
    arg3K0 = "#t";
    goto L12334;}}
 L12324: {
  if ((3 == (3 & thing_949X))) {
    if ((2 == (31 & ((((*((long *) ((((char *) (-3 + thing_949X))) + -4))))>>2))))) {
      arg3K0 = "#(...)";
      goto L12334;}
    else {
      goto L12328;}}
  else {
    goto L12328;}}
 L17589: {
  SvalS = (*((long *) ((((char *) (-3 + (Sinterrupt_handlersS)))) + (((i_1089X)<<2)))));
  obj_1211X = SvalS;
  if ((3 == (3 & obj_1211X))) {
    if ((3 == (31 & ((((*((long *) ((((char *) (-3 + obj_1211X))) + -4))))>>2))))) {
      goto L17606;}
    else {
      goto L17692;}}
  else {
    goto L17692;}}
 L16635: {
  arg1K0 = (Scode_pointerS);
  goto L19093;}
 L16642: {
  v_1212X = arg0K0;
  merged_arg0K0 = v_1212X;
  copy_stack_into_heap_return_tag = 3;
  goto copy_stack_into_heap;
 copy_stack_into_heap_return_3:
  if ((stack_slots_1160X < (64 + (((((SstackS) - (Sstack_limitS)))>>2))))) {
    goto L16631;}
  else {
    ps_error("VM's stack is too small (how can this happen?)", 0);
    goto L16631;}}
 L37531: {
  length_1213X = arg0K0;
  if ((length_1213X < (start_1178X + count_1179X))) {
    arg0K0 = 7;
    goto L37394;}
  else {
    got_1214X = ps_read_fd(((((*((long *) ((((char *) (-3 + x_1119X))) + 8))))>>2)), ((((char *) (-3 + arg5_1023X))) + start_1178X), count_1179X, waitP_1177X, &eofP_1215X, &pendingP_1216X, &status_1217X);
    if ((status_1217X == NO_ERRORS)) {
      if (eofP_1215X) {
        SvalS = 21;
        Scode_pointerS = ((Scode_pointerS) + 1);
        arg1K0 = (Scode_pointerS);
        goto L19093;}
      else {
        if (pendingP_1216X) {
          addr_1218X = (((char *) (-3 + x_1119X))) + 16;
          S48_WRITE_BARRIER(x_1119X, addr_1218X, 5);
          *((long *) addr_1218X) = 5;
          s48_Spending_interruptPS = 0;
          Senabled_interruptsS = 0;
          arg0K0 = 13;
          goto L37394;}
        else {
          SvalS = (((got_1214X)<<2));
          Scode_pointerS = ((Scode_pointerS) + 1);
          arg1K0 = (Scode_pointerS);
          goto L19093;}}}
    else {
      push_exception_continuationB(25, 1);
      *((long *) (SstackS)) = (((status_1217X)<<2));
      SstackS = ((SstackS) + -4);
      *((long *) (SstackS)) = arg5_1023X;
      SstackS = ((SstackS) + -4);
      *((long *) (SstackS)) = (((start_1178X)<<2));
      SstackS = ((SstackS) + -4);
      *((long *) (SstackS)) = (((count_1179X)<<2));
      SstackS = ((SstackS) + -4);
      if (waitP_1177X) {
        arg0K0 = 5;
        goto L37450;}
      else {
        arg0K0 = 1;
        goto L37450;}}}}
 L37527: {
  arg0K0 = ((long)(((unsigned long)(*((long *) ((((char *) (-3 + arg5_1023X))) + -4))))>>8));
  goto L37531;}
 L37409: {
  x_1219X = arg0K0;
  *((long *) (SstackS)) = x_1219X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = x_1119X;
  SstackS = ((SstackS) + -4);
  arg0K0 = 5;
  goto L17320;}
 L12328: {
  if ((3 == (3 & thing_949X))) {
    if ((3 == (31 & ((((*((long *) ((((char *) (-3 + thing_949X))) + -4))))>>2))))) {
      arg3K0 = "#{procedure}";
      goto L12334;}
    else {
      arg3K0 = "???";
      goto L12334;}}
  else {
    arg3K0 = "???";
    goto L12334;}}
 L17606: {
  StemplateS = (SvalS);
  Slosing_opcodeS = (0 - i_1089X);
  arg0K0 = arg_count_1152X;
  goto L16721;}
 L17692: {
  ps_error("interrupt handler is not a closure", 1, i_1089X);
  goto L17606;}
 L37450: {
  x_1220X = arg0K0;
  *((long *) (SstackS)) = x_1220X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = x_1119X;
  SstackS = ((SstackS) + -4);
  merged_arg0K0 = status_1217X;
  merged_arg0K1 = key_1019X;
  get_error_string_return_tag = 5;
  goto get_error_string;
 get_error_string_return_5:
  x_1221X = get_error_string0_return_value;
  *((long *) (SstackS)) = x_1221X;
  SstackS = ((SstackS) + -4);
  arg0K0 = 7;
  goto L17320;}
 loseD0: {
  message_386X = merged_arg3K0;{
  why_1222X = (((*((long *) ((SstackS) + (4 + (((nargs_756X)<<2)))))))>>2);
  ps_write_string("Template UIDs: ", (stderr));
  current_template_1223X = StemplateS;
  out_1224X = stderr;
  if ((0 == (3 & (*((long *) ((((char *) (-3 + current_template_1223X))) + 4)))))) {
    if ((current_template_1223X == (*((long *) ((((char *) (-3 + (Sbottom_of_stackS)))) + 8))))) {
      arg2K0 = 0;
      goto L11158;}
    else {
      ps_write_integer(((((*((long *) ((((char *) (-3 + current_template_1223X))) + 4))))>>2)), out_1224X);
      arg2K0 = 1;
      goto L11158;}}
  else {
    arg2K0 = 0;
    goto L11158;}}
 L11158: {
  not_firstP_1225X = arg2K0;
  arg0K0 = (ScontS);
  arg2K1 = not_firstP_1225X;
  goto L11165;}
 L11165: {
  cont_1226X = arg0K0;
  not_firstP_1227X = arg2K1;
  if ((3 == (3 & cont_1226X))) {
    if ((10 == (31 & ((((*((long *) ((((char *) (-3 + cont_1226X))) + -4))))>>2))))) {
      template_1228X = *((long *) ((((char *) (-3 + cont_1226X))) + 8));
      if ((0 == (3 & (*((long *) ((((char *) (-3 + template_1228X))) + 4)))))) {
        if ((template_1228X == (*((long *) ((((char *) (-3 + (Sbottom_of_stackS)))) + 8))))) {
          arg2K0 = not_firstP_1227X;
          goto L11174;}
        else {
          if (not_firstP_1227X) {
            ps_write_string(" <- ", out_1224X);
            goto L11213;}
          else {
            goto L11213;}}}
      else {
        arg2K0 = not_firstP_1227X;
        goto L11174;}}
    else {
      goto L17339;}}
  else {
    goto L17339;}}
 L11174: {
  v_1229X = arg2K0;
  arg0K0 = (*((long *) (((char *) (-3 + cont_1226X)))));
  arg2K1 = v_1229X;
  goto L11165;}
 L11213: {
  ps_write_integer(((((*((long *) ((((char *) (-3 + template_1228X))) + 4))))>>2)), out_1224X);
  arg2K0 = 1;
  goto L11174;}
 L17339: {
  { long ignoreXX;
  PS_WRITE_CHAR(10, (stderr), ignoreXX) }
  if ((why_1222X == 1)) {
    if ((0 == (3 & (*((long *) (((char *) (-3 + (*((long *) ((SstackS) + (((nargs_756X)<<2))))))))))))) {
      ps_error(message_386X, 3, opcode_757X, why_1222X, ((((*((long *) (((char *) (-3 + (*((long *) ((SstackS) + (((nargs_756X)<<2)))))))))))>>2)));
      goto loseD0_return;}
    else {
      goto L17363;}}
  else {
    goto L17363;}}
 L17363: {
  ps_error(message_386X, 2, opcode_757X, why_1222X);
  goto loseD0_return;}
 loseD0_return:
  switch (loseD0_return_tag) {
  case 0: goto loseD0_return_0;
  default: goto loseD0_return_1;
  }}

 check_events: {
{ goto L40893;}
 L40893: {
  type_1230X = s48_get_next_event(&channel_1231X, &status_1232X);
  if ((type_1230X == ALARM_EVENT)) {
    if ((1 == (Sinterrupted_templateS))) {
      Sinterrupted_templateS = (StemplateS);
      arg0K0 = 1;
      goto L16517;}
    else {
      arg0K0 = 1;
      goto L16517;}}
  else {
    if ((type_1230X == KEYBOARD_INTERRUPT_EVENT)) {
      arg0K0 = 2;
      goto L16517;}
    else {
      if ((type_1230X == IO_READ_COMPLETION_EVENT)) {
        enqueue_input_channelB(channel_1231X, status_1232X);
        arg0K0 = 8;
        goto L16517;}
      else {
        if ((type_1230X == IO_WRITE_COMPLETION_EVENT)) {
          enqueue_output_channelB(channel_1231X, status_1232X);
          arg0K0 = 16;
          goto L16517;}
        else {
          if ((type_1230X == OS_SIGNAL_EVENT)) {
            arg0K0 = 32;
            goto L16517;}
          else {
            if ((type_1230X == NO_EVENT)) {
              arg0K0 = 0;
              goto L16517;}
            else {
              if ((type_1230X == ERROR_EVENT)) {
                ps_write_string("OS error while getting event", (stderr));
                { long ignoreXX;
                PS_WRITE_CHAR(10, (stderr), ignoreXX) }
                ps_write_string((ps_error_string(status_1232X)), (stderr));
                { long ignoreXX;
                PS_WRITE_CHAR(10, (stderr), ignoreXX) }
                arg0K0 = 0;
                goto L16517;}
              else {
                ps_write_string("unknown type of event", (stderr));
                { long ignoreXX;
                PS_WRITE_CHAR(10, (stderr), ignoreXX) }
                arg0K0 = 0;
                goto L16517;}}}}}}}}
 L16517: {
  interrupt_bit_1233X = arg0K0;
  Spending_interruptsS = ((Spending_interruptsS) | interrupt_bit_1233X);
  if ((type_1230X == NO_EVENT)) {
    if ((0 == ((Spending_interruptsS) & (Senabled_interruptsS)))) {
      s48_Spending_interruptPS = 0;
      if ((s48_Spending_eventsPS)) {
        s48_Spending_interruptPS = 1;
        goto L16547;}
      else {
        goto L16547;}}
    else {
      s48_Spending_interruptPS = 1;
      goto L16547;}}
  else {
    goto L40893;}}
 L16547: {
  if ((s48_Spending_interruptPS)) {
    if ((s48_Spending_eventsPS)) {
      s48_Spending_eventsPS = 0;
      goto L40893;}
    else {
      check_events0_return_value = 1;
      goto check_events_return;}}
  else {
    check_events0_return_value = 0;
    goto check_events_return;}}
 check_events_return:
  switch (check_events_return_tag) {
  case 0: goto check_events_return_0;
  case 1: goto check_events_return_1;
  case 2: goto check_events_return_2;
  default: goto check_events_return_3;
  }}

 rest_list_setup: {
  wants_stack_args_382X = merged_arg0K0;
  stack_arg_count_383X = merged_arg0K1;
  list_args_384X = merged_arg0K2;
  list_arg_count_385X = merged_arg0K3;{
  if ((stack_arg_count_383X == wants_stack_args_382X)) {
    merged_arg0K0 = list_args_384X;
    merged_arg0K1 = list_arg_count_385X;
    copy_listS_return_tag = 1;
    goto copy_listS;
   copy_listS_return_1:
    x_1234X = copy_listS0_return_value;
    *((long *) (SstackS)) = x_1234X;
    SstackS = ((SstackS) + -4);
    goto rest_list_setup_return;}
  else {
    if ((stack_arg_count_383X < wants_stack_args_382X)) {
      count_1235X = wants_stack_args_382X - stack_arg_count_383X;
      merged_arg0K0 = list_args_384X;
      merged_arg0K1 = count_1235X;
      push_list_return_tag = 3;
      goto push_list;
     push_list_return_3:
      v_1236X = push_list0_return_value;
      merged_arg0K0 = v_1236X;
      merged_arg0K1 = (list_arg_count_385X - count_1235X);
      copy_listS_return_tag = 2;
      goto copy_listS;
     copy_listS_return_2:
      x_1237X = copy_listS0_return_value;
      *((long *) (SstackS)) = x_1237X;
      SstackS = ((SstackS) + -4);
      goto rest_list_setup_return;}
    else {
      merged_arg0K0 = list_args_384X;
      merged_arg0K1 = list_arg_count_385X;
      copy_listS_return_tag = 3;
      goto copy_listS;
     copy_listS_return_3:
      v_1238X = copy_listS0_return_value;
      merged_arg0K0 = v_1238X;
      merged_arg0K1 = (stack_arg_count_383X - wants_stack_args_382X);
      pop_args_GlistS_return_tag = 5;
      goto pop_args_GlistS;
     pop_args_GlistS_return_5:
      x_1239X = pop_args_GlistS0_return_value;
      *((long *) (SstackS)) = x_1239X;
      SstackS = ((SstackS) + -4);
      goto rest_list_setup_return;}}}
 rest_list_setup_return:
  switch (rest_list_setup_return_tag) {
  case 0: goto rest_list_setup_return_0;
  case 1: goto rest_list_setup_return_1;
  default: goto rest_list_setup_return_2;
  }}

 push_list: {
  list_380X = merged_arg0K0;
  count_381X = merged_arg0K1;{
  *((long *) (SstackS)) = list_380X;
  SstackS = ((SstackS) + -4);
  if ((count_381X < (64 + (((((SstackS) - (Sstack_limitS)))>>2))))) {
    goto L15933;}
  else {
    space_1240X = 1 + (((((Sstack_endS) - (SstackS)))>>2));
    v_1241X = AVAILABLEp(space_1240X);
    if (v_1241X) {
      arg2K0 = 1;
      arg0K1 = 0;
      goto L16017;}
    else {
      collect_saving_temps(1, 1, &temp1_1242X);
      v_1243X = AVAILABLEp(space_1240X);
      if (v_1243X) {
        arg2K0 = 1;
        arg0K1 = 0;
        goto L16017;}
      else {
        arg2K0 = 0;
        arg0K1 = 0;
        goto L16017;}}}}
 L15933: {
  SstackS = ((SstackS) + 4);
  list_1244X = *((long *) (SstackS));
  arg0K0 = count_381X;
  arg0K1 = list_1244X;
  goto L15942;}
 L16017: {
  okayP_1245X = arg2K0;
  key_1246X = arg0K1;
  if (okayP_1245X) {
    arg0K0 = key_1246X;
    goto L15968;}
  else {
    ps_error("Scheme48 heap overflow", 0);
    arg0K0 = key_1246X;
    goto L15968;}}
 L15942: {
  i_1247X = arg0K0;
  l_1248X = arg0K1;
  if ((0 < i_1247X)) {
    *((long *) (SstackS)) = (*((long *) (((char *) (-3 + l_1248X)))));
    SstackS = ((SstackS) + -4);
    arg0K0 = (-1 + i_1247X);
    arg0K1 = (*((long *) ((((char *) (-3 + l_1248X))) + 4)));
    goto L15942;}
  else {
    push_list0_return_value = l_1248X;
    goto push_list_return;}}
 L15968: {
  v_1249X = arg0K0;
  merged_arg0K0 = v_1249X;
  copy_stack_into_heap_return_tag = 4;
  goto copy_stack_into_heap;
 copy_stack_into_heap_return_4:
  if ((count_381X < (64 + (((((SstackS) - (Sstack_limitS)))>>2))))) {
    goto L15933;}
  else {
    ps_error("VM's stack is too small (how can this happen?)", 0);
    goto L15933;}}
 push_list_return:
  switch (push_list_return_tag) {
  case 0: goto push_list_return_0;
  case 1: goto push_list_return_1;
  case 2: goto push_list_return_2;
  default: goto push_list_return_3;
  }}

 copy_stack_into_heap: {
  key_379X = merged_arg0K0;{
  arg1K0 = ((SstackS) + 4);
  arg0K1 = 0;
  goto L14156;}
 L14156: {
  p_1250X = arg1K0;
  i_1251X = arg0K1;
  if ((2 == (3 & (*((long *) p_1250X))))) {
    if ((26 == (*((long *) p_1250X)))) {
      arg0K0 = (-1 + i_1251X);
      goto L14143;}
    else {
      arg0K0 = i_1251X;
      goto L14143;}}
  else {
    arg1K0 = (p_1250X + 4);
    arg0K1 = (1 + i_1251X);
    goto L14156;}}
 L14143: {
  arg_count_1252X = arg0K0;
  top_1253X = SstackS;
  if ((1 == (ScontS))) {
    goto L14148;}
  else {
    merged_arg0K0 = key_379X;
    merged_arg0K1 = 1;
    really_preserve_continuation_return_tag = 1;
    goto really_preserve_continuation;
   really_preserve_continuation_return_1:
    goto L14148;}}
 L14148: {
  stob_1254X = ScontS;
  arg1K0 = ((((char *) (-3 + stob_1254X))) + -8);
  arg1K1 = (top_1253X + (((arg_count_1252X)<<2)));
  goto L14181;}
 L14181: {
  loc_1255X = arg1K0;
  arg_1256X = arg1K1;
  if ((top_1253X < arg_1256X)) {
    *((long *) loc_1255X) = (*((long *) arg_1256X));
    arg1K0 = (loc_1255X + -4);
    arg1K1 = (arg_1256X + -4);
    goto L14181;}
  else {
    SstackS = loc_1255X;
    goto copy_stack_into_heap_return;}}
 copy_stack_into_heap_return:
  switch (copy_stack_into_heap_return_tag) {
  case 0: goto copy_stack_into_heap_return_0;
  case 1: goto copy_stack_into_heap_return_1;
  case 2: goto copy_stack_into_heap_return_2;
  case 3: goto copy_stack_into_heap_return_3;
  default: goto copy_stack_into_heap_return_4;
  }}

 really_preserve_continuation: {
  key_377X = merged_arg0K0;
  reason_378X = merged_arg0K1;{
  p_1257X = SenvS;
  if ((3 == (3 & p_1257X))) {
    if ((p_1257X < (((long) (Sstack_beginS))))) {
      goto L13332;}
    else {
      if (((((long) (Sstack_endS))) < p_1257X)) {
        goto L13332;}
      else {
        merged_arg0K0 = (SenvS);
        merged_arg0K1 = (ScontS);
        merged_arg0K2 = key_377X;
        merged_arg0K3 = reason_378X;
        save_env_in_heap_return_tag = 1;
        goto save_env_in_heap;
       save_env_in_heap_return_1:
        v_1258X = save_env_in_heap0_return_value;
        SenvS = v_1258X;
        goto L13332;}}}
  else {
    goto L13332;}}
 L13332: {
  end_1259X = *((long *) (((char *) (-3 + (Sbottom_of_stackS)))));
  arg0K0 = (ScontS);
  arg0K1 = (Sbottom_of_stackS);
  goto L13285;}
 L13285: {
  cont_1260X = arg0K0;
  previous_1261X = arg0K1;
  if ((cont_1260X == (Sbottom_of_stackS))) {
    *((long *) (((char *) (-3 + previous_1261X)))) = end_1259X;
    ScontS = (Sbottom_of_stackS);
    really_preserve_continuation0_return_value = (*((long *) (((char *) (-3 + (Sbottom_of_stackS))))));
    goto really_preserve_continuation_return;}
  else {
    p_1262X = *((long *) ((((char *) (-3 + cont_1260X))) + 12));
    if ((3 == (3 & p_1262X))) {
      if ((p_1262X < (((long) (Sstack_beginS))))) {
        goto L13303;}
      else {
        if (((((long) (Sstack_endS))) < p_1262X)) {
          goto L13303;}
        else {
          merged_arg0K0 = (*((long *) ((((char *) (-3 + cont_1260X))) + 12)));
          merged_arg0K1 = cont_1260X;
          merged_arg0K2 = key_377X;
          merged_arg0K3 = reason_378X;
          save_env_in_heap_return_tag = 2;
          goto save_env_in_heap;
         save_env_in_heap_return_2:
          goto L13303;}}}
    else {
      goto L13303;}}}
 L13303: {
  header_1263X = *((long *) ((((char *) (-3 + cont_1260X))) + -4));
  addr_1264X = ALLOCATE_SPACE((31 & (((header_1263X)>>2))), (4 + ((long)(((unsigned long)header_1263X)>>8))));
  data_addr_1265X = addr_1264X + 4;
  *((long *) addr_1264X) = header_1263X;
  memcpy((void *)data_addr_1265X, (void *)(((char *) (-3 + cont_1260X))),((long)(((unsigned long)header_1263X)>>8)));
  new_1266X = 3 + (((long) data_addr_1265X));
  *((long *) (((char *) (-3 + previous_1261X)))) = new_1266X;
  arg0K0 = (*((long *) (((char *) (-3 + new_1266X)))));
  arg0K1 = new_1266X;
  goto L13285;}
 really_preserve_continuation_return:
  switch (really_preserve_continuation_return_tag) {
  case 0: goto really_preserve_continuation_return_0;
  default: goto really_preserve_continuation_return_1;
  }}

 copy_env: {
  env_376X = merged_arg0K0;{
  header_1267X = *((long *) ((((char *) (-3 + env_376X))) + -4));
  addr_1268X = ALLOCATE_SPACE((31 & (((header_1267X)>>2))), (4 + ((long)(((unsigned long)header_1267X)>>8))));
  data_addr_1269X = addr_1268X + 4;
  *((long *) addr_1268X) = header_1267X;
  memcpy((void *)data_addr_1269X, (void *)(((char *) (-3 + env_376X))),((long)(((unsigned long)header_1267X)>>8)));
  new_1270X = 3 + (((long) data_addr_1269X));
  addr_1271X = ((char *) (-3 + env_376X));
  S48_WRITE_BARRIER(env_376X, addr_1271X, 26);
  *((long *) addr_1271X) = 26;
  *((long *) ((((char *) (-3 + env_376X))) + -4)) = new_1270X;
  copy_env0_return_value = new_1270X;
  goto copy_env_return;}
 copy_env_return:
  switch (copy_env_return_tag) {
  case 0: goto copy_env_return_0;
  default: goto copy_env_return_1;
  }}

 save_env_in_heap: {
  env_372X = merged_arg0K0;
  cont_373X = merged_arg0K1;
  key_374X = merged_arg0K2;
  reason_375X = merged_arg0K3;{
  merged_arg0K0 = env_372X;
  merged_arg0K1 = key_374X;
  merged_arg0K2 = reason_375X;
  copy_env_return_tag = 0;
  goto copy_env;
 copy_env_return_0:
  top_1272X = copy_env0_return_value;
  arg0K0 = top_1272X;
  goto L12564;}
 L12564: {
  env_1273X = arg0K0;
  p_1274X = *((long *) (((char *) (-3 + env_1273X))));
  if ((3 == (3 & p_1274X))) {
    if ((p_1274X < (((long) (Sstack_beginS))))) {
      goto L12583;}
    else {
      if (((((long) (Sstack_endS))) < p_1274X)) {
        goto L12583;}
      else {
        merged_arg0K0 = (*((long *) (((char *) (-3 + env_1273X)))));
        merged_arg0K1 = key_374X;
        merged_arg0K2 = reason_375X;
        copy_env_return_tag = 1;
        goto copy_env;
       copy_env_return_1:
        new_1275X = copy_env0_return_value;
        addr_1276X = ((char *) (-3 + env_1273X));
        S48_WRITE_BARRIER(env_1273X, addr_1276X, new_1275X);
        *((long *) addr_1276X) = new_1275X;
        arg0K0 = new_1275X;
        goto L12564;}}}
  else {
    goto L12583;}}
 L12583: {
  arg0K0 = cont_373X;
  goto L12587;}
 L12587: {
  cont_1277X = arg0K0;
  env_1278X = *((long *) ((((char *) (-3 + cont_1277X))) + 12));
  if ((3 == (3 & env_1278X))) {
    if ((3 == (3 & (*((long *) ((((char *) (-3 + env_1278X))) + -4)))))) {
      *((long *) ((((char *) (-3 + cont_1277X))) + 12)) = (*((long *) ((((char *) (-3 + env_1278X))) + -4)));
      arg0K0 = (*((long *) (((char *) (-3 + cont_1277X)))));
      goto L12587;}
    else {
      save_env_in_heap0_return_value = top_1272X;
      goto save_env_in_heap_return;}}
  else {
    save_env_in_heap0_return_value = top_1272X;
    goto save_env_in_heap_return;}}
 save_env_in_heap_return:
  switch (save_env_in_heap_return_tag) {
  case 0: goto save_env_in_heap_return_0;
  case 1: goto save_env_in_heap_return_1;
  default: goto save_env_in_heap_return_2;
  }}

 pop_args_GlistS: {
  start_370X = merged_arg0K0;
  count_371X = merged_arg0K1;{
  space_1279X = 3 * count_371X;
  v_1280X = AVAILABLEp(space_1279X);
  if (v_1280X) {
    arg2K0 = 1;
    arg0K1 = start_370X;
    goto L14902;}
  else {
    temp0_1281X = collect_saving_temps(start_370X, 1, &temp1_1282X);
    v_1283X = AVAILABLEp(space_1279X);
    if (v_1283X) {
      arg2K0 = 1;
      arg0K1 = temp0_1281X;
      goto L14902;}
    else {
      arg2K0 = 0;
      arg0K1 = temp0_1281X;
      goto L14902;}}}
 L14902: {
  okayP_1284X = arg2K0;
  temp_1285X = arg0K1;
  if (okayP_1284X) {
    arg0K0 = temp_1285X;
    goto L14876;}
  else {
    ps_error("Scheme48 heap overflow", 0);
    arg0K0 = temp_1285X;
    goto L14876;}}
 L14876: {
  start_1286X = arg0K0;
  arg0K0 = start_1286X;
  arg0K1 = count_371X;
  goto L14885;}
 L14885: {
  args_1287X = arg0K0;
  count_1288X = arg0K1;
  if ((0 == count_1288X)) {
    pop_args_GlistS0_return_value = args_1287X;
    goto pop_args_GlistS_return;}
  else {
    SstackS = ((SstackS) + 4);
    a_1289X = *((long *) (SstackS));
    addr_1290X = ALLOCATE_SPACE(0, 12);
    *((long *) addr_1290X) = 2050;
    x_1291X = 3 + (((long) (addr_1290X + 4)));
    *((long *) (((char *) (-3 + x_1291X)))) = a_1289X;
    *((long *) ((((char *) (-3 + x_1291X))) + 4)) = args_1287X;
    arg0K0 = x_1291X;
    arg0K1 = (-1 + count_1288X);
    goto L14885;}}
 pop_args_GlistS_return:
  switch (pop_args_GlistS_return_tag) {
  case 0: goto pop_args_GlistS_return_0;
  case 1: goto pop_args_GlistS_return_1;
  case 2: goto pop_args_GlistS_return_2;
  case 3: goto pop_args_GlistS_return_3;
  case 4: goto pop_args_GlistS_return_4;
  default: goto pop_args_GlistS_return_5;
  }}

 copy_listS: {
  list_368X = merged_arg0K0;
  length_369X = merged_arg0K1;{
  if ((0 == length_369X)) {
    copy_listS0_return_value = 25;
    goto copy_listS_return;}
  else {
    space_1292X = 3 * length_369X;
    v_1293X = AVAILABLEp(space_1292X);
    if (v_1293X) {
      arg2K0 = 1;
      arg0K1 = list_368X;
      goto L14781;}
    else {
      temp0_1294X = collect_saving_temps(list_368X, 1, &temp1_1295X);
      v_1296X = AVAILABLEp(space_1292X);
      if (v_1296X) {
        arg2K0 = 1;
        arg0K1 = temp0_1294X;
        goto L14781;}
      else {
        arg2K0 = 0;
        arg0K1 = temp0_1294X;
        goto L14781;}}}}
 L14781: {
  okayP_1297X = arg2K0;
  temp_1298X = arg0K1;
  if (okayP_1297X) {
    arg0K0 = temp_1298X;
    goto L14739;}
  else {
    ps_error("Scheme48 heap overflow", 0);
    arg0K0 = temp_1298X;
    goto L14739;}}
 L14739: {
  list_1299X = arg0K0;
  a_1300X = *((long *) (((char *) (-3 + list_1299X))));
  addr_1301X = ALLOCATE_SPACE(0, 12);
  *((long *) addr_1301X) = 2050;
  x_1302X = 3 + (((long) (addr_1301X + 4)));
  *((long *) (((char *) (-3 + x_1302X)))) = a_1300X;
  *((long *) ((((char *) (-3 + x_1302X))) + 4)) = 25;
  arg0K0 = (*((long *) ((((char *) (-3 + list_1299X))) + 4)));
  arg0K1 = x_1302X;
  goto L14754;}
 L14754: {
  l_1303X = arg0K0;
  last_1304X = arg0K1;
  if ((25 == l_1303X)) {
    copy_listS0_return_value = x_1302X;
    goto copy_listS_return;}
  else {
    a_1305X = *((long *) (((char *) (-3 + l_1303X))));
    addr_1306X = ALLOCATE_SPACE(0, 12);
    *((long *) addr_1306X) = 2050;
    x_1307X = 3 + (((long) (addr_1306X + 4)));
    *((long *) (((char *) (-3 + x_1307X)))) = a_1305X;
    *((long *) ((((char *) (-3 + x_1307X))) + 4)) = 25;
    addr_1308X = (((char *) (-3 + last_1304X))) + 4;
    S48_WRITE_BARRIER(last_1304X, addr_1308X, x_1307X);
    *((long *) addr_1308X) = x_1307X;
    arg0K0 = (*((long *) ((((char *) (-3 + l_1303X))) + 4)));
    arg0K1 = x_1307X;
    goto L14754;}}
 copy_listS_return:
  switch (copy_listS_return_tag) {
  case 0: goto copy_listS_return_0;
  case 1: goto copy_listS_return_1;
  case 2: goto copy_listS_return_2;
  default: goto copy_listS_return_3;
  }}

 collect_saving_temp: {
  value_367X = merged_arg0K0;{
  value_1309X = collect_saving_temps(value_367X, 0, &v_1310X);
  collect_saving_temp0_return_value = value_1309X;
  goto collect_saving_temp_return;}
 collect_saving_temp_return:
  switch (collect_saving_temp_return_tag) {
  case 0: goto collect_saving_temp_return_0;
  default: goto collect_saving_temp_return_1;
  }}

 copy_continuation_from_heapB: {
  cont_366X = merged_arg0K0;{
  top_1311X = (((char *) (-3 + (Sbottom_of_stackS)))) + (-8 - (-4 & (3 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + cont_366X))) + -4))))>>8)))));
  new_cont_1312X = 3 + (((long) (top_1311X + 4)));
  SstackS = (top_1311X + -4);
  ScontS = new_cont_1312X;
  v_1313X = (((3 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + cont_366X))) + -4))))>>8))))>>2);
  memcpy((void *)top_1311X, (void *)((((char *) (-3 + cont_366X))) + -4),(4 + (((v_1313X)<<2))));
  *((long *) (((char *) (-3 + (Sbottom_of_stackS))))) = (*((long *) (((char *) (-3 + new_cont_1312X)))));
  *((long *) (((char *) (-3 + new_cont_1312X)))) = (Sbottom_of_stackS);
  copy_continuation_from_heapB0_return_value = new_cont_1312X;
  goto copy_continuation_from_heapB_return;}
 copy_continuation_from_heapB_return:
  switch (copy_continuation_from_heapB_return_tag) {
  case 0: goto copy_continuation_from_heapB_return_0;
  default: goto copy_continuation_from_heapB_return_1;
  }}

 get_current_port: {
  marker_365X = merged_arg0K0;{
  thread_1314X = Scurrent_threadS;
  if ((3 == (3 & thread_1314X))) {
    if ((9 == (31 & ((((*((long *) ((((char *) (-3 + thread_1314X))) + -4))))>>2))))) {
      if ((1 < ((((3 + ((long)(((unsigned long)(*((long *) ((((char *) (-3 + thread_1314X))) + -4))))>>8))))>>2)))) {
        arg0K0 = (*((long *) ((((char *) (-3 + thread_1314X))) + 4)));
        goto L10854;}
      else {
        goto L10904;}}
    else {
      goto L10904;}}
  else {
    goto L10904;}}
 L10854: {
  env_1315X = arg0K0;
  if ((3 == (3 & env_1315X))) {
    if ((0 == (31 & ((((*((long *) ((((char *) (-3 + env_1315X))) + -4))))>>2))))) {
      obj_1316X = *((long *) (((char *) (-3 + env_1315X))));
      if ((3 == (3 & obj_1316X))) {
        if ((0 == (31 & ((((*((long *) ((((char *) (-3 + obj_1316X))) + -4))))>>2))))) {
          if ((marker_365X == (*((long *) (((char *) (-3 + (*((long *) (((char *) (-3 + env_1315X)))))))))))) {
            get_current_port0_return_value = (*((long *) ((((char *) (-3 + (*((long *) (((char *) (-3 + env_1315X)))))))) + 4)));
            goto get_current_port_return;}
          else {
            arg0K0 = (*((long *) ((((char *) (-3 + env_1315X))) + 4)));
            goto L10854;}}
        else {
          goto L10926;}}
      else {
        goto L10926;}}
    else {
      goto L10926;}}
  else {
    goto L10926;}}
 L10904: {
  ps_error("current thread is not a record", 0);
  get_current_port0_return_value = v_1317X;
  goto get_current_port_return;}
 L10926: {
  if ((25 == env_1315X)) {
    if (((((marker_365X)>>2)) == 1)) {
      arg3K0 = "dynamic environment doesn't have current-output-port";
      goto L10880;}
    else {
      arg3K0 = "dynamic environment doesn't have current-input-port";
      goto L10880;}}
  else {
    ps_error("dynamic environment is not a proper list", 0);
    get_current_port0_return_value = v_1318X;
    goto get_current_port_return;}}
 L10880: {
  v_1319X = arg3K0;
  ps_error(v_1319X, 0);
  get_current_port0_return_value = v_1320X;
  goto get_current_port_return;}
 get_current_port_return:
  switch (get_current_port_return_tag) {
  case 0: goto get_current_port_return_0;
  case 1: goto get_current_port_return_1;
  default: goto get_current_port_return_2;
  }}

 okay_argument_list: {
  list_364X = merged_arg0K0;{
  arg0K0 = list_364X;
  arg0K1 = 0;
  arg0K2 = list_364X;
  arg2K3 = 0;
  goto L10721;}
 L10721: {
  fast_1321X = arg0K0;
  len_1322X = arg0K1;
  slow_1323X = arg0K2;
  move_slowP_1324X = arg2K3;
  if ((25 == fast_1321X)) {
    okay_argument_list0_return_value = 1;
    okay_argument_list1_return_value = len_1322X;
    goto okay_argument_list_return;}
  else {
    if ((3 == (3 & fast_1321X))) {
      if ((0 == (31 & ((((*((long *) ((((char *) (-3 + fast_1321X))) + -4))))>>2))))) {
        if (move_slowP_1324X) {
          if ((fast_1321X == slow_1323X)) {
            okay_argument_list0_return_value = 0;
            okay_argument_list1_return_value = 0;
            goto okay_argument_list_return;}
          else {
            arg0K0 = (*((long *) ((((char *) (-3 + fast_1321X))) + 4)));
            arg0K1 = (1 + len_1322X);
            arg0K2 = (*((long *) ((((char *) (-3 + slow_1323X))) + 4)));
            arg2K3 = 0;
            goto L10721;}}
        else {
          arg0K0 = (*((long *) ((((char *) (-3 + fast_1321X))) + 4)));
          arg0K1 = (1 + len_1322X);
          arg0K2 = slow_1323X;
          arg2K3 = 1;
          goto L10721;}}
      else {
        okay_argument_list0_return_value = 0;
        okay_argument_list1_return_value = 0;
        goto okay_argument_list_return;}}
    else {
      okay_argument_list0_return_value = 0;
      okay_argument_list1_return_value = 0;
      goto okay_argument_list_return;}}}
 okay_argument_list_return:
  switch (okay_argument_list_return_tag) {
  case 0: goto okay_argument_list_return_0;
  default: goto okay_argument_list_return_1;
  }}

 get_error_string: {
  status_363X = merged_arg0K0;{
  string_1325X = ps_error_string(status_363X);
  x_1326X = strlen((char *) string_1325X);
  if ((x_1326X < 256)) {
    arg0K0 = x_1326X;
    goto L10228;}
  else {
    arg0K0 = 256;
    goto L10228;}}
 L10228: {
  len_1327X = arg0K0;
  len_1328X = 1 + len_1327X;
  addr_1329X = ALLOCATE_SPACE(17, (4 + len_1328X));
  *((long *) addr_1329X) = (70 + (((len_1328X)<<8)));
  string_1330X = 3 + (((long) (addr_1329X + 4)));
  *((unsigned char *) ((((char *) (-3 + string_1330X))) + len_1327X)) = 0;
  arg0K0 = 0;
  goto L10238;}
 L10238: {
  i_1331X = arg0K0;
  if ((i_1331X == len_1327X)) {
    get_error_string0_return_value = string_1330X;
    goto get_error_string_return;}
  else {
    *((unsigned char *) ((((char *) (-3 + string_1330X))) + i_1331X)) = ((*(string_1325X + i_1331X)));
    arg0K0 = (1 + i_1331X);
    goto L10238;}}
 get_error_string_return:
  switch (get_error_string_return_tag) {
  case 0: goto get_error_string_return_0;
  case 1: goto get_error_string_return_1;
  case 2: goto get_error_string_return_2;
  case 3: goto get_error_string_return_3;
  case 4: goto get_error_string_return_4;
  default: goto get_error_string_return_5;
  }}

 pop_continuationB: {
{ cont_1332X = ScontS;
  tem_1333X = *((long *) ((((char *) (-3 + cont_1332X))) + 8));
  pc_1334X = *((long *) ((((char *) (-3 + cont_1332X))) + 4));
  StemplateS = tem_1333X;
  Scode_pointerS = ((((char *) (-3 + (*((long *) (((char *) (-3 + tem_1333X)))))))) + (((pc_1334X)>>2)));
  SenvS = (*((long *) ((((char *) (-3 + cont_1332X))) + 12)));
  ScontS = (*((long *) (((char *) (-3 + cont_1332X)))));
  SstackS = ((((char *) (-3 + cont_1332X))) + 12);
  goto pop_continuationB_return;}
 pop_continuationB_return:
  switch (pop_continuationB_return_tag) {
  case 0: goto pop_continuationB_return_0;
  case 1: goto pop_continuationB_return_1;
  default: goto pop_continuationB_return_2;
  }}

}
long s48_call_startup_procedure(char **startup_vector_1335X, long startup_vector_length_1336X)
{
  char arg2K0;
  long arg0K1;
  long arg0K0;
  long v_1385X;
  long x_1384X;
  long x_1383X;
  char * addr_1382X;
  long b_1381X;
  long channel_1380X;
  long x_1379X;
  char * addr_1378X;
  long b_1377X;
  long channel_1376X;
  long x_1375X;
  char * addr_1374X;
  long b_1373X;
  long channel_1372X;
  long key_1371X;
  long key_1370X;
  char okayP_1369X;
  char v_1368X;
  long temp1_1367X;
  char v_1366X;
  long space_1365X;
  long i_1364X;
  long length_1363X;
  long *v_1362X;
  long v_1361X;
  long v_1360X;
  long y_1359X;
  long x_1358X;
  long v_1357X;
  long x_1356X;
  long y_1355X;
  char * addr_1354X;
  long value_1353X;
  long x_1352X;
  long y_1351X;
  long i_1350X;
  long vector_1349X;
  char * addr_1348X;
  long len_1347X;
  long key_1346X;
  long key_1345X;
  char okayP_1344X;
  char v_1343X;
  long temp1_1342X;
  char v_1341X;
  long space_1340X;
  long size_1339X;
  long i_1338X;
  long tem_1337X;
 {  SstackS = ((((char *) (-3 + (Sbottom_of_stackS)))) + -8);
  *((long *) (((char *) (-3 + (Sbottom_of_stackS))))) = 1;
  ScontS = (Sbottom_of_stackS);
  SenvS = 13;
  tem_1337X = Sinterrupt_templateS;
  StemplateS = tem_1337X;
  Scode_pointerS = (((char *) (-3 + (*((long *) (((char *) (-3 + tem_1337X))))))));
  SvalS = 13;
  Scurrent_threadS = 25;
  Ssession_dataS = 25;
  Sexception_handlersS = 25;
  Sinterrupt_handlersS = 25;
  Senabled_interruptsS = 0;
  Sfinalizer_alistS = 25;
  Sfinalize_theseS = 25;
  Spending_interruptsS = 0;
  s48_Spending_interruptPS = 0;
  Sinterrupted_templateS = 1;
  arg0K0 = 0;
  arg0K1 = 0;
  goto L16251;}
 L16251: {
  i_1338X = arg0K0;
  size_1339X = arg0K1;
  if ((i_1338X == startup_vector_length_1336X)) {
    space_1340X = 1 + (size_1339X + startup_vector_length_1336X);
    v_1341X = AVAILABLEp(space_1340X);
    if (v_1341X) {
      arg2K0 = 1;
      arg0K1 = 0;
      goto L16288;}
    else {
      collect_saving_temps(1, 1, &temp1_1342X);
      v_1343X = AVAILABLEp(space_1340X);
      if (v_1343X) {
        arg2K0 = 1;
        arg0K1 = 0;
        goto L16288;}
      else {
        arg2K0 = 0;
        arg0K1 = 0;
        goto L16288;}}}
  else {
    arg0K0 = (1 + i_1338X);
    arg0K1 = (1 + (size_1339X + ((((4 + (strlen((char *) (*(startup_vector_1335X + i_1338X))))))>>2))));
    goto L16251;}}
 L16288: {
  okayP_1344X = arg2K0;
  key_1345X = arg0K1;
  if (okayP_1344X) {
    arg0K0 = key_1345X;
    goto L16266;}
  else {
    ps_error("Scheme48 heap overflow", 0);
    arg0K0 = key_1345X;
    goto L16266;}}
 L16266: {
  key_1346X = arg0K0;
  len_1347X = ((startup_vector_length_1336X)<<2);
  addr_1348X = ALLOCATE_SPACE(2, (4 + len_1347X));
  *((long *) addr_1348X) = (10 + (((len_1347X)<<8)));
  vector_1349X = 3 + (((long) (addr_1348X + 4)));
  arg0K0 = 0;
  goto L16324;}
 L16324: {
  i_1350X = arg0K0;
  if ((i_1350X == startup_vector_length_1336X)) {
    *((long *) (SstackS)) = vector_1349X;
    SstackS = ((SstackS) + -4);
    y_1351X = fileno((stderr));
    x_1352X = fileno((stdout));
    if ((x_1352X < y_1351X)) {
      arg0K0 = y_1351X;
      goto L15154;}
    else {
      arg0K0 = x_1352X;
      goto L15154;}}
  else {
    value_1353X = enter_string((*(startup_vector_1335X + i_1350X)), key_1346X);
    addr_1354X = (((char *) (-3 + vector_1349X))) + (((i_1350X)<<2));
    S48_WRITE_BARRIER(vector_1349X, addr_1354X, value_1353X);
    *((long *) addr_1354X) = value_1353X;
    arg0K0 = (1 + i_1350X);
    goto L16324;}}
 L15154: {
  y_1355X = arg0K0;
  x_1356X = fileno((stdin));
  if ((x_1356X < y_1355X)) {
    arg0K0 = y_1355X;
    goto L15156;}
  else {
    arg0K0 = x_1356X;
    goto L15156;}}
 L15156: {
  v_1357X = arg0K0;
  x_1358X = Snumber_of_channelsS;
  y_1359X = 1 + v_1357X;
  if ((x_1358X < y_1359X)) {
    arg0K0 = y_1359X;
    goto L15158;}
  else {
    arg0K0 = x_1358X;
    goto L15158;}}
 L15158: {
  v_1360X = arg0K0;
  Snumber_of_channelsS = v_1360X;
  v_1361X = fileno((stdin));
  Svm_channelsS = ((long*)malloc(sizeof(long) * (Snumber_of_channelsS)));
  Spending_input_channels_headS = 1;
  Spending_input_channels_tailS = 1;
  Spending_output_channels_headS = 1;
  Spending_output_channels_tailS = 1;
  if ((NULL == (Svm_channelsS))) {
    ps_error("out of memory, unable to continue", 0);
    goto L15186;}
  else {
    goto L15186;}}
 L15186: {
  v_1362X = Svm_channelsS;
  length_1363X = Snumber_of_channelsS;
  arg0K0 = 0;
  goto L15234;}
 L15234: {
  i_1364X = arg0K0;
  if ((i_1364X < length_1363X)) {
    *(v_1362X + i_1364X) = 1;
    arg0K0 = (1 + i_1364X);
    goto L15234;}
  else {
    space_1365X = 3 * (7 + ((((4 + (strlen((char *) "standard output"))))>>2)));
    v_1366X = AVAILABLEp(space_1365X);
    if (v_1366X) {
      arg2K0 = 1;
      arg0K1 = 0;
      goto L15248;}
    else {
      collect_saving_temps(1, 1, &temp1_1367X);
      v_1368X = AVAILABLEp(space_1365X);
      if (v_1368X) {
        arg2K0 = 1;
        arg0K1 = 0;
        goto L15248;}
      else {
        arg2K0 = 0;
        arg0K1 = 0;
        goto L15248;}}}}
 L15248: {
  okayP_1369X = arg2K0;
  key_1370X = arg0K1;
  if (okayP_1369X) {
    arg0K0 = key_1370X;
    goto L15192;}
  else {
    ps_error("Scheme48 heap overflow", 0);
    arg0K0 = key_1370X;
    goto L15192;}}
 L15192: {
  key_1371X = arg0K0;
  channel_1372X = fileno((stdin));
  b_1373X = enter_string("standard input", key_1371X);
  addr_1374X = ALLOCATE_SPACE(6, 24);
  *((long *) addr_1374X) = 5146;
  x_1375X = 3 + (((long) (addr_1374X + 4)));
  *((long *) (((char *) (-3 + x_1375X)))) = 4;
  *((long *) ((((char *) (-3 + x_1375X))) + 4)) = b_1373X;
  *((long *) ((((char *) (-3 + x_1375X))) + 8)) = (((channel_1372X)<<2));
  *((long *) ((((char *) (-3 + x_1375X))) + 12)) = 1;
  *((long *) ((((char *) (-3 + x_1375X))) + 16)) = 1;
  *((Svm_channelsS) + channel_1372X) = x_1375X;
  channel_1376X = fileno((stderr));
  b_1377X = enter_string("standard error", key_1371X);
  addr_1378X = ALLOCATE_SPACE(6, 24);
  *((long *) addr_1378X) = 5146;
  x_1379X = 3 + (((long) (addr_1378X + 4)));
  *((long *) (((char *) (-3 + x_1379X)))) = 8;
  *((long *) ((((char *) (-3 + x_1379X))) + 4)) = b_1377X;
  *((long *) ((((char *) (-3 + x_1379X))) + 8)) = (((channel_1376X)<<2));
  *((long *) ((((char *) (-3 + x_1379X))) + 12)) = 1;
  *((long *) ((((char *) (-3 + x_1379X))) + 16)) = 1;
  *((Svm_channelsS) + channel_1376X) = x_1379X;
  channel_1380X = fileno((stdout));
  b_1381X = enter_string("standard output", key_1371X);
  addr_1382X = ALLOCATE_SPACE(6, 24);
  *((long *) addr_1382X) = 5146;
  x_1383X = 3 + (((long) (addr_1382X + 4)));
  *((long *) (((char *) (-3 + x_1383X)))) = 8;
  *((long *) ((((char *) (-3 + x_1383X))) + 4)) = b_1381X;
  *((long *) ((((char *) (-3 + x_1383X))) + 8)) = (((channel_1380X)<<2));
  *((long *) ((((char *) (-3 + x_1383X))) + 12)) = 1;
  *((long *) ((((char *) (-3 + x_1383X))) + 16)) = 1;
  *((Svm_channelsS) + channel_1380X) = x_1383X;
  *((long *) (SstackS)) = x_1375X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = x_1383X;
  SstackS = ((SstackS) + -4);
  *((long *) (SstackS)) = x_1379X;
  SstackS = ((SstackS) + -4);
  x_1384X = s48_resumer_records();
  *((long *) (SstackS)) = x_1384X;
  SstackS = ((SstackS) + -4);
  s48_initialization_completeB();
  v_1385X = s48_startup_procedure();
  return s48_restart(v_1385X, 5);}
}void
s48_init(void)
{
Snumber_of_channelsS = 100;
Spending_input_channels_headS = 1;
Spending_input_channels_tailS = 1;
Spending_output_channels_headS = 1;
Spending_output_channels_tailS = 1;
Sstack_warningPS = 1;
Slosing_opcodeS = 0;
Sos_signal_listS = 25;
Sexternal_exceptionPS = 0;
Sexternal_root_stackS = NULL;
Sexternal_root_stack_baseS = NULL;
Spermanent_external_rootsS = NULL;
Sgc_root_procS = HtopD9001;
Spost_gc_cleanupS = HtopD8994;
s48_Scallback_return_stack_blockS = 1;
s48_Spending_eventsPS = 0;
}

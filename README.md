# MSEC — Securities Operations End-of-Day Batch

Source libraries of the z/OS end-of-day batch application, unloaded from the partitioned datasets into one file per member. Each folder name matches the source library it came from.

| Folder | z/OS library | Content |
|---|---|---|
| `src/cobol` | MSEC.PROD.COBOL | Enterprise COBOL batch programs, called modules and DB2 I/O modules |
| `src/copy` | MSEC.PROD.COPYLIB | Copybooks: record layouts, linkage areas, DCLGENs |
| `src/jcl` | MSEC.PROD.JCL | Production, initialization and ad hoc jobs |
| `src/proc` | MSEC.PROD.PROCLIB | Cataloged procedures |
| `src/ctl` | MSEC.PROD.CTLCARD | SYSIN control cards: DFSORT, IDCAMS, DSNUTILB, TSO batch, program parameters |
| `src/asm` | MSEC.PROD.ASM | HLASM subroutines |
| `src/ddl` | MSEC.PROD.DDL | DB2 DDL and grants |
| `src/sched` | Control-M | Scheduler export (`MSEC_EOD.xml`) and calendars (`CALENDARS.xml`) |

Naming conventions:

* Programs are `<ss><t><nnn>`. The subsystem codes `ss` are CM, TC, SR, CA, MG, RC, RR and SW. The type `t` is B batch, R report, U called utility, D DB2 I/O module.
* Jobs are `MS<ss><f><nnn>`. The frequency `f` is D daily, M month-end, I initialization, X ad hoc.

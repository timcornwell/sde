#include "pvm3.h"
#include "pvmtev.h"

void sde_initxpvm_()

{

Pvmtmask trace_mask; 

int xpvm_tid; 

/* Get XPVM Task ID */ 

if ( (xpvm_tid = pvm_gettid( "xpvm", 0 )) > 0 ) 

      {
	/* Set Self Trace & Output Destinations & Message Codes */ 
	pvm_setopt( PvmSelfTraceTid, xpvm_tid ); 
	pvm_setopt( PvmSelfTraceCode, 666 ); 
	pvm_setopt( PvmSelfOutputTid, xpvm_tid ); 
	pvm_setopt( PvmSelfOutputCode, 667 ); 

	/* Set Future Children's Trace & Output Dests & Codes */ 
	/* (optional) */ 
	pvm_setopt( PvmTraceTid, xpvm_tid ); 
	pvm_setopt( PvmTraceCode, 666 ); 
	pvm_setopt( PvmOutputTid, xpvm_tid ); 
	pvm_setopt( PvmOutputCode, 667 ); 

	/* Generate Default Trace Mask */ 
	TEV_INIT_MASK( trace_mask ); 
	TEV_SET_MASK( trace_mask, TEV_MCAST0 ); 
	TEV_SET_MASK( trace_mask, TEV_SEND0 ); 
	TEV_SET_MASK( trace_mask, TEV_RECV0 ); 
	TEV_SET_MASK( trace_mask, TEV_NRECV0 ); 

	/* Add Other Desired Events Here */ 

	/* Set Self Trace Mask */ 
	pvm_settmask( PvmTaskSelf, trace_mask ); 

	/* Set Future Children's Trace Mask */ 
	/* (optional) */ 
	pvm_settmask( PvmTaskChild, trace_mask ); 
      }

  else
	printf( "XPVM not running, cannot trace\n" ); 

}

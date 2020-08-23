// Custom Terms used for the US Supreme Court Citation Network
// See: https://github.com/desmarais-lab/Supreme_Court_Citation_Network


#include "changestats.users.h"

D_CHANGESTAT_FN(d_difftransties){
  Edge e, f;
  int i, echange, ochange;
  int L2th, L2tu, L2uh;
  Vertex tail, head, u, v;
  double cumchange;
  double tailattr, headattr;

  CHANGE_STAT[0] = 0.0;

  /* *** don't forget tail -> head */
  FOR_EACH_TOGGLE(i){
    cumchange=0.0;
    L2th=0;
    ochange = IS_OUTEDGE(tail=TAIL(i), head=HEAD(i)) ? -1 : 0;
    echange = 2*ochange + 1;
      tailattr = INPUT_ATTRIB[tail-1];
      headattr = INPUT_ATTRIB[head-1];
      /* no attributes */
            /* step through outedges of head  */
            STEP_THROUGH_OUTEDGES(head, e, u){
              if (IS_OUTEDGE(tail, u) && (tailattr != headattr) && (tailattr != INPUT_ATTRIB[u-1]) ){
                L2tu=ochange;
                /* step through inedges of u */
                STEP_THROUGH_INEDGES(u, f, v){
                  if(IS_OUTEDGE(tail, v) && (tailattr != INPUT_ATTRIB[v-1]) ){
                    L2tu++;
                    if(L2tu>0) {break;}
                  }
                }
                cumchange += (L2tu==0);
              }
            }
      /* step through inedges of head */
      STEP_THROUGH_INEDGES(head, e, u){
        if (IS_OUTEDGE(tail, u) && (tailattr != headattr) && (tailattr != INPUT_ATTRIB[u-1])){
          L2th++;
        }
        if (IS_OUTEDGE(u, tail) && (tailattr != INPUT_ATTRIB[u-1]) && (headattr != INPUT_ATTRIB[u-1])  ){
          L2uh=ochange;
          /* step through outedges of u */
          STEP_THROUGH_OUTEDGES(u, f, v){
            if(IS_OUTEDGE(v, head) && (INPUT_ATTRIB[v-1] != INPUT_ATTRIB[u-1])){
              L2uh++;
              if(L2uh>0) {break;}
            }
          }
          cumchange += (L2uh==0) ;
        }
      }

    cumchange += (L2th>0) ;
    cumchange  = echange*cumchange;
    (CHANGE_STAT[0]) += cumchange;
    TOGGLE_IF_MORE_TO_COME(i);
  }
  UNDO_PREVIOUS_TOGGLES(i);
}


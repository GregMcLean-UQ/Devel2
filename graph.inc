* ====================================================================
      common /graph_logical/
     .   init,
     .   xaxis,
     .   yaxis,
     .   offscale
     
      common /graph_integer/
     .   CharX,
     .   CharY,
     .   DivsX,
     .   DivsY

      common /graph_char/
     .   Buffer
     
      common /graph_real/
     .   XStart,
     .   XInt,
     .   YStart,
     .   YInt
* ====================================================================

*   Short description:
*     Common block for all graph variables

*   Notes:

*   Changes:
*     DPH 14/6/95

*   Calls:

* ----------------------- Declaration section ------------------------

*   Constant variables

*   Global variables

      logical Init
      logical XAxis
      logical YAxis
      logical OffScale
      integer CharX
      integer CharY
      integer DivsX
      integer DivsY
      character Buffer(140,140)*1
      real XStart
      real XInt
      real YStart
      real YInt
      

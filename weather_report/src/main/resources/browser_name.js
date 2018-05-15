var isIE   = /*@cc_on!@*/false || !!document.documentMode
var isEdge = !isIE && !!window.StyleMedia

var browser_name =
  (navigator.userAgent.indexOf("Chrome") != -1 && !isEdge)
  ? 'chrome'
  : (navigator.userAgent.indexOf("Safari") != -1 && !isEdge)
    ? 'safari'
    : (navigator.userAgent.indexOf("Firefox") != -1 )
      ? browser_name = 'firefox'
      : ((navigator.userAgent.indexOf("MSIE") != -1 ) ||
         (!!document.documentMode )) //IF IE > 10
        ? browser_name = 'ie'
        : isEdge
          ? 'edge'
          : 'unknown'

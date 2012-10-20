<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<% java.util.Map templateData = (java.util.Map)request.getAttribute("templateData"); %>
<html>
  <head>
    <title>Calculator</title>
  </head>

  <body>
    <h1>Calculator</h1>

    Result: <%= templateData.get("result") %>

    <hr>
    <address><a href="mailto:matthias@sorted.org">Matthias Radestock</a></address>
<!-- Created: Fri Mar  8 12:25:33 GMT 2002 -->
<!-- hhmts start -->
Last modified: Mon Aug 26 21:38:34 BST 2002
<!-- hhmts end -->
  </body>
</html>

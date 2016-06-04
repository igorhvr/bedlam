<%
response.setStatus(301);
response.setHeader( "Location", "s/index.html" );
response.setHeader( "Connection", "close" );
%>

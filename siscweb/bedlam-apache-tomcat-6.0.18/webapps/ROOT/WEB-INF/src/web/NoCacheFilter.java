package web;

import javax.servlet.*;
import java.io.PrintWriter;

public class NoCacheFilter implements javax.servlet.Filter {
    public void init(FilterConfig filterConfig) throws ServletException {}

    public void doFilter(ServletRequest request,
			 ServletResponse response,
			 FilterChain chain)
	throws java.io.IOException, ServletException {    
        //PrintWriter w = response.getWriter();
        //w.append("Cache-Control: no-cache");
        //w.append("Pragma: no-cache");
        //w.append("Expires: 0");
	    //chain.doFilter(request,response);
        //response.setContentType("text/html\nCache-Control: no-cache\n");
        response.getOutputStream().write("Cache-Control: no-cache".getBytes());
        chain.doFilter(request,response);
    }

    public void destroy() {}
}

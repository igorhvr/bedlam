package web;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;

import java.io.IOException;
import java.io.PrintWriter;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpServletResponseWrapper;
import java.util.regex.Pattern;

import java.util.Map;

public class RewriteResponseHeaderFilter implements Filter {
    private static Pattern htmlMatcher =Pattern.compile(".+/[^/]+\\.html");

    public void doFilter(ServletRequest request,
                         ServletResponse response,
                         FilterChain chain) throws IOException, ServletException {
        ResponseWrapper newResponse = new ResponseWrapper((HttpServletResponse) response);

        // We now enable content a security policy[1][2] that allows content from iasylum.net and all its subdomains.
	//
        // [1]. http://w3c.github.io/webappsec/specs/content-security-policy/csp-specification.dev.html
        // [2]. https://developer.mozilla.org/en-US/docs/Security/CSP/Using_Content_Security_Policy
		
	//newResponse.setHeader("Content-Security-Policy", "default-src 'self' *.iasylum.net");
        newResponse.setHeader("RewriteResponseHeaderFilter", "activated");	
        newResponse.setHeader("RewriteResponseHeaderFilterVersion", "2014-03-29");	
        newResponse.setHeader("Cache-Control", "no-store");
        newResponse.setHeader("Pragma", "no-cache");
        newResponse.setHeader("Expires", "0");

	Object forcedResponseHeaders = iu.M.d.get("forcedResponseHeadersMap");
	if(forcedResponseHeaders != null) {
	    java.util.Map forcedResponseHeadersMap = (java.util.Map) forcedResponseHeaders;

	    for(Object e : forcedResponseHeadersMap.entrySet()) {
		newResponse.setHeader(((Map.Entry)e).getKey().toString(), 
				      ((Map.Entry)e).getValue().toString());
	    }
	}
        
        newResponse.setCharacterEncoding("UTF-8");

        HttpServletRequestWrapper requestWrapper = new HttpServletRequestWrapper((HttpServletRequest)request);

        if(htmlMatcher.matcher(requestWrapper.getServletPath()).matches()) {
            PrintWriter w = newResponse.getWriter();
            w.println("<HTML><HEAD>");
            w.println("<META HTTP-EQUIV=\"PRAGMA\" CONTENT=\"NO-CACHE\">");
            w.println("<META HTTP-EQUIV=\"Expires\" CONTENT=\"-1\">");
            w.println("</HEAD>");
        }

        chain.doFilter(request, newResponse);


    }


    class ResponseWrapper extends HttpServletResponseWrapper {
        public ResponseWrapper(HttpServletResponse response) {
            super(response);
        }
    }

    public void init(FilterConfig filterConfig)
            throws ServletException {
    }

    public void destroy() {
    }
}
                                                  

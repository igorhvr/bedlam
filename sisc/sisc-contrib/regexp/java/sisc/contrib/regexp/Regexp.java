/* 
   Regexp.java

   Sisc module for Jakarta ORO binding.

   Author: Ovidiu Predescu <ovidiu@cup.hp.com>
   Date: December 14, 2001

   Copyright (C) 2001 Ovidiu Predescu
   All rights reserved.

   The contents of this file are subject to the Mozilla Public License
   Version 1.1 (the "License"); you may not use this file except in
   compliance with the License. You may obtain a copy of the License
   at http://www.mozilla.org/MPL/
  
   Software distributed under the License is distributed on an "AS IS"
   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
   the License for the specific language governing rights and
   limitations under the License.

   The Original Code is the Second Interpreter of Scheme Code (SISC).

   The Initial Developer of the Original Code is Scott G. Miller.
   Portions created by Scott G. Miller are Copyright (C) 2000-2001
   Scott G. Miller.  All Rights Reserved.

   Alternatively, the contents of this file may be used under the
   terms of the GNU General Public License Version 2 or later (the
   "GPL"), in which case the provisions of the GPL are applicable
   instead of those above.  If you wish to allow use of your version
   of this file only under the terms of the GPL and not to allow
   others to use your version of this file under the MPL, indicate
   your decision by deleting the provisions above and replace them
   with the notice and other provisions required by the GPL.  If you
   do not delete the provisions above, a recipient may use your
   version of this file under either the MPL or the GPL.
 */
package sisc.contrib.regexp;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.util.ArrayList;
import org.apache.oro.text.GlobCompiler;
import org.apache.oro.text.awk.AwkCompiler;
import org.apache.oro.text.awk.AwkMatcher;
import org.apache.oro.text.perl.Perl5Util;
import org.apache.oro.text.regex.MalformedPatternException;
import org.apache.oro.text.regex.MatchResult;
import org.apache.oro.text.regex.Pattern;
import org.apache.oro.text.regex.PatternMatcher;
import org.apache.oro.text.regex.PatternMatcherInput;
import org.apache.oro.text.regex.Perl5Compiler;
import org.apache.oro.text.regex.Perl5Matcher;
import org.apache.oro.text.regex.StringSubstitution;
import org.apache.oro.text.regex.Util;
import sisc.interpreter.ContinuationException;
import sisc.interpreter.Interpreter;
import sisc.nativefun.*;
import sisc.data.*;
import sisc.io.ValueWriter;

/**
 * <p>Implements an interface to the Jakarta ORO regular expression
 * engine.
 *
 * <p>The Scheme API is composed of the following functions:
 *
 * <ul>
 *  <li><b>regexp/pattern</b>
 *      <it>string</it> [<it>type</it> <it>options</it>] -> <it>pattern</it><br>
 *
 *      <p>Compiles a string representing a regular expression into a
 *      pattern object. <it>type</it> is a symbol describing the type
 *      of regular expression; currently supported types are
 *      <it>glob</it>, <it>perl5</it>, or <it>awk</it>. If no regexp
 *      type is specified, the default one is <it>perl5</it>.
 *
 *      <p>If the <it>options</it> argument is present, it is a list
 *      composed of any of the following symbols:
 *
 *      <ul>
 *       <li>case-insensitive</it>
 *       <li>multiline</it>
 *      </ul>
 *
 *  <li><b>regexp/match</b>
 *      <it>string</it> <it>pattern</it> -> #f | list of <it>matches</it><br>
 *
 *      <p>Try to match <it>pattern</it> on the input string. If
 *      there's no match <tt>#f</tt> is returned, otherwise a list of
 *      <it>matches</it> is returned. A <it>match</it> is a cons cell
 *      containing the beginning and end positions of a match group.
 *
 * </ul>
 *
 * @author <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
 * @since December 14, 2001
 *
 * @see org.apache.oro.text.regex.Pattern
 * @see org.apache.oro.text.regex.PatternCompiler
 * @see org.apache.oro.text.regex.PatternMatcher
 */
public class Regexp extends IndexedProcedure
{
 
  public Regexp(int id) {
  	super(id);
  }
  
  public static final int RPATTERN = 1, RMATCH = 2, RMATCH_POSITIONS = 3,
    RREPLACE = 4, RREPLACE_ALL = 5, RSPLIT = 6, RSPLIT_DELIM = 7;

  public static final Symbol
    REGEX_PERL5 = Symbol.get("perl5"),
    REGEX_GLOB = Symbol.get("glob"),
    REGEX_AWK = Symbol.get("awk"),
    CASE_INSENSITIVE = Symbol.get("case-insensitive"),
    EXTENDED = Symbol.get("extended"),
    SINGLELINE = Symbol.get("singleline"),
    MULTILINE = Symbol.get("multiline");

  public static final int optionsFromScheme(Value value, Value type)
  {
    if (value instanceof Symbol) {
      if (value == CASE_INSENSITIVE) {
        if (type == REGEX_PERL5)
          return Perl5Compiler.CASE_INSENSITIVE_MASK;
        else if (type == REGEX_GLOB)
          return GlobCompiler.CASE_INSENSITIVE_MASK;
        else if (type == REGEX_AWK)
          return AwkCompiler.CASE_INSENSITIVE_MASK;
        else
          throw new RuntimeException("Unknown compiler " + type);
      }
      else if (value == EXTENDED) {
        if (type == REGEX_PERL5)
          return Perl5Compiler.EXTENDED_MASK;
        else
          throw new RuntimeException("The extended mask is supported only by Perl5 regexps");
      }
      else if (value == SINGLELINE) {
        if (type == REGEX_PERL5)
          return Perl5Compiler.SINGLELINE_MASK;
        else
          throw new RuntimeException("The singleline mask is supported only by Perl5 regexps");
      }
      else if (value == MULTILINE) {
        if (type == REGEX_PERL5)
          return Perl5Compiler.MULTILINE_MASK;
        else if (type == REGEX_GLOB)
          throw new RuntimeException("Glob compiler doesn't support this option: " + value);
        else if (type == REGEX_AWK)
          return AwkCompiler.MULTILINE_MASK;
      }
      else
        throw new RuntimeException("Unsupported regexp option " + value);
    }

    else if (value instanceof Pair) {
      int options = 0;
      Pair pv = (Pair)value;

      while (pv != EMPTYLIST) {
          options |= optionsFromScheme(pv.car(), type);
          pv = (Pair)pv.cdr();
      }

      return options;
    }

    else
      throw new RuntimeException("Invalid format for options " + value);

    // Not reached, but keeps the Java compiler happy
    return 0;
  }

  public static class Index extends IndexedLibraryAdapter {

		public String getLibraryName()
		{
	   	  return "Jakarta ORO regexp";
	 	}

	 	public float getLibraryVersion()
	 	{
	   		return 1.0f;
	 	}

		public Value construct(Object context, int id) {
		 return new Regexp(id);
		}
        
		 public Index() {
		    define("regexp", RPATTERN);
    		define("regexp-match", RMATCH);
    		define("regexp-match-positions", RMATCH_POSITIONS);
		    define("regexp-replace", RREPLACE);
    		define("regexp-replace*", RREPLACE_ALL);
		    define("regexp-split", RSPLIT);
    		define("regexp-split/delimiter", RSPLIT_DELIM);
  		}
  }
  
  protected static RPattern patternFor(Value v)
  {
    RPattern pat;

    if (v instanceof RPattern)
      pat = (RPattern)v;
    else
      pat = new RPattern(string(v));

    return pat;
  }

  public Value doApply(Interpreter r)
    throws ContinuationException
  {
    switch (r.vlr.length) {

      // One argument functions
    case 1: 
      if (id == RPATTERN)
        return new RPattern(string(r.vlr[0]), REGEX_PERL5, 0);
      else
        break;

      // Two argument functions
    case 2:
      switch (id) {
      case RPATTERN:
        return new RPattern(string(r.vlr[0]), r.vlr[1], 0);
      case RMATCH:
        return patternFor(r.vlr[0]).match(r.vlr[1]);
      case RMATCH_POSITIONS:
        return patternFor(r.vlr[0]).matchPositions(r.vlr[1]);
      case RSPLIT:
        return RPattern.splitNoDelimiters(r.vlr[0], r.vlr[1]);
      case RSPLIT_DELIM:
        return patternFor(r.vlr[0]).splitWithDelimiters(r.vlr[1]);
      default:
        break;
      }

      // Three argument functions
    case 3:
      switch (id) {
      case RPATTERN:
        return new RPattern(string(r.vlr[0]),
                            r.vlr[1],
                            optionsFromScheme(r.vlr[2], r.vlr[1]));
      case RREPLACE: 
        return patternFor(r.vlr[0]).replaceFirst(r.vlr[1], r.vlr[2]);
      case RREPLACE_ALL: 
        return patternFor(r.vlr[0]).replaceAll(r.vlr[1], r.vlr[2]);
      default:
        break;
      }
    }

    throw new RuntimeException("Invalid number of arguments to function "
                               + r.acc);
  }

  public static class RPattern extends Value  {    
    public Pattern pattern;
    Symbol type;
    int options;

    public RPattern() {}

    public RPattern(String pat)
    {
      setup (pat, REGEX_PERL5, 0);
    }

    public RPattern(String pat, Value type, int options)
    {
      setup (pat, type, options);
    }

    public void setup(String pat, Value type, int options)
    {
      try {
        if (type == REGEX_PERL5)
          pattern = (new Perl5Compiler()).compile(pat, options);
        else if (type == REGEX_GLOB)
          pattern = (new GlobCompiler()).compile(pat, options);
        else if (type == REGEX_AWK)
          pattern = (new AwkCompiler()).compile(pat, options);
        else
          throw new RuntimeException("unkown regular expression type: "
                                     + type);
      }
      catch (MalformedPatternException ex) {
        throw new RuntimeException("Malformed pattern: " + pat + "\n" + ex);
      }
      this.type = (Symbol)type;
      this.options = options;
    }

    protected PatternMatcher getMatcher()
    {
      if (type == REGEX_PERL5 || type == REGEX_GLOB)
        return new Perl5Matcher();
      else if (type == REGEX_AWK)
        return new AwkMatcher();
      else
        throw new RuntimeException("Unknown regular expression type: " + type);
    }

    public Value match(Value str)
    {
      PatternMatcher matcher = getMatcher();

      // Do the matching
      PatternMatcherInput jStr
        = new PatternMatcherInput(string(str));
      Pair result = null;
      Pair prev = null;
      boolean found = false;
      
      while (matcher.contains(jStr, pattern)) {
        found = true;

        MatchResult matchResult = matcher.getMatch();

        for (int i = 0, length = matchResult.groups(); i < length; i++) {
          Pair m = new Pair(new SchemeString(matchResult.group(i)), EMPTYLIST);
          if (result == null)
              result = prev = m;
          else {
              prev.setCdr(m);
              prev = m;
          }
        }
      }

      if (!found)
        return FALSE;
      else
        return result;
    }

    public Value matchPositions(Value str)
    {
      PatternMatcher matcher = getMatcher();

      // Do the matching
      PatternMatcherInput jStr
        = new PatternMatcherInput(string(str));
      Pair result = null;
      Pair prev = null;
      boolean found = false;

      while (matcher.contains(jStr, pattern)) {
        found = true;

        MatchResult matchResult = matcher.getMatch();

        for (int i = 0, length = matchResult.groups(); i < length; i++) {
          Pair m = new Pair(Quantity.valueOf(matchResult.beginOffset(i)),
                            Quantity.valueOf(matchResult.endOffset(i)));
          Pair elem = new Pair(m, EMPTYLIST);
          if (result == null)
              result = prev = elem;
          else {
              prev.setCdr(elem);
              prev = elem;
          }
        }
      }

      if (!found)
        return FALSE;
      else
        return result;
    }

    public Value replaceFirst(Value input, Value substitution)
    {
      String result =
        Util.substitute(getMatcher(), pattern,
                        new StringSubstitution(string(substitution)),
                        string(input));
      return new SchemeString(result);
    }

    public Value replaceAll(Value input, Value substitution)
    {
      String result =
        Util.substitute(getMatcher(), pattern,
                        new StringSubstitution(string(substitution)),
                        string(input),
                        Util.SUBSTITUTE_ALL);
      return new SchemeString(result);
    }

    public static Value splitNoDelimiters(Value pat, Value input)
    {
      String pattern;
      
      if (pat instanceof RPattern)
          pattern = ((RPattern)pat).pattern.getPattern();
      else
          pattern = string(pat);

      ArrayList list = new ArrayList();
      (new Perl5Util()).split(list, pattern, string(input));
      SchemeVector result = new SchemeVector(list.size());
      for (int i = 0, length = list.size(); i < length; i++)
        result.vals[i] = new SchemeString((String)list.get(i));
      return result;
    }

    public Value splitWithDelimiters(Value input)
    {
      try {
        ArrayList list = new ArrayList();
        Util.split(list, getMatcher(), pattern, string(input));
        SchemeVector result = new SchemeVector(list.size());
        for (int i = 0, length = list.size(); i < length; i++)
          result.vals[i] = new SchemeString((String)list.get(i));
        return result;
      }
      catch (Exception ex) {
        System.out.println("error: " + ex);
        ex.printStackTrace();
        throw new RuntimeException(ex.toString());
      }
    }

    public void display(ValueWriter w) throws IOException
    {
        w.append("#<regexp ")
            .append(pattern.getClass().getName())
            .append(" '")
            .append(pattern.getPattern().toString())
            .append("'>");
    }

    public boolean valueEqual(Value ov)
    {
        return (ov instanceof RPattern) &&
            pattern.equals(((RPattern)ov).pattern);
    }

    public void serialize(sisc.ser.Serializer s) throws IOException
    {
        s.writeExpression(type);
        s.writeUTF(pattern.getPattern());
        s.writeInt(pattern.getOptions());
    }

    public void deserialize(sisc.ser.Deserializer s) throws IOException
    {
        type = (Symbol)s.readExpression();
        String pat = s.readUTF();
        options = s.readInt();
        setup(pat, type, options);
    }
  }
}

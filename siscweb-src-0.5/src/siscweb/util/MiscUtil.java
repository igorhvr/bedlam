/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is SISCweb.
 *
 * The Initial Developer of the Original Code is Alessandro Colomba.
 * Portions created by the Alessandro Colomba are Copyright (C) 2005-2006
 * Alessandro Colomba. All Rights Reserved.
 *
 * Contributor(s):
 * Dan Muresan
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** */

package siscweb.util;

import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.util.Iterator;

import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.ImageWriter;
import javax.imageio.stream.ImageInputStream;
import javax.imageio.stream.ImageOutputStream;

import sisc.data.Procedure;
import sisc.data.SchemeString;
import sisc.data.Symbol;
import sisc.data.Value;
import sisc.interpreter.AppContext;
import sisc.interpreter.Context;
import sisc.interpreter.Interpreter;
import sisc.interpreter.SchemeCaller;
import sisc.interpreter.SchemeException;
import sisc.util.Util;


public class MiscUtil
{
    public static String qualifyName(String context, String name)
    {
        return (context != null && !"".equals(context.trim()))
             ? context + "::" + name
             : name;
    }



    public static void copyStream(InputStream is, OutputStream os)
        throws IOException
    {
        final int BUF_SIZE = 10000;
        byte[] buffer = new byte[BUF_SIZE];

        for(int length = -1; (length = is.read(buffer, 0, BUF_SIZE)) != -1;) {
            os.write(buffer, 0, length);
        }
    }


    public static BufferedImage readImage(String type, InputStream is)
        throws IOException
    {
        Iterator readers = ImageIO.getImageReadersByFormatName(type);
        ImageReader reader = (ImageReader)readers.next();


        ImageInputStream iis = ImageIO.createImageInputStream(is);
        reader.setInput(iis, true);

        return reader.read(0);
    }

    public static void writeImage(String type, RenderedImage image, OutputStream os)
        throws IOException
    {
        Iterator writers = ImageIO.getImageWritersByFormatName(type);
        ImageWriter writer = (ImageWriter)writers.next();

        ImageOutputStream ios = ImageIO.createImageOutputStream(os);

        writer.setOutput(ios);
        writer.write(image);
    }

    public static void eval(final String sexp,
                            final URL path,
                            final AppContext appContext,
                            final String contextName)
        throws SchemeException
    {
        try {
            if(sexp != null) {
                Context.execute(appContext, new SchemeCaller() {
                        public Object execute(Interpreter r)
                            throws SchemeException
                        {
                            try {
                                if(path != null) {
                                    final Procedure currentDirectory =
                                        (Procedure) r.lookup(Symbol.get("current-url"),
                                                             Util.TOPLEVEL);
                                    r.eval(currentDirectory,
                                           new Value[] {
                                               new SchemeString(path.toString())
                                           });
                                }

                                return r.eval(sexp);
                            }
                            catch(final IOException ioe) {
                                throw new RuntimeException(ioe);
                            }
                        } });
            }
        }
        catch(final SchemeException se) {
            Context.execute(new SchemeCaller() {
                    public Object execute(Interpreter r)
                        throws SchemeException
                    {
                        final Procedure printError = (Procedure) r.lookup(Symbol.get("print-error"), Util.TOPLEVEL);

                        return r.eval(printError, new Value[] {se.m, se.e});
                    } });

            throw se;
        }
    }
}

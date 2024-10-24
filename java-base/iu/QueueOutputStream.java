package iu;

import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.TimeUnit;
import sisc.data.SchemeString;

public class QueueOutputStream extends OutputStream {
    private final BlockingQueue<SchemeString> queue;
    private final String charsetName;
    private final long offerTimeout; // in milliseconds

    public QueueOutputStream(BlockingQueue<SchemeString> queue, String charsetName, long offerTimeout) {
        this.queue = queue;
        this.charsetName = (charsetName != null) ? charsetName : "UTF-8";
        this.offerTimeout = offerTimeout;
    }

    public void write(int b) throws IOException {
        byte[] data = new byte[] { (byte) b };
        enqueueData(data);
    }

    public void write(byte[] b) throws IOException {
        enqueueData(b);
    }

    public void write(byte[] b, int off, int len) throws IOException {
        byte[] data = new byte[len];
        System.arraycopy(b, off, data, 0, len);
        enqueueData(data);
    }

    private void enqueueData(byte[] data) throws IOException {
        SchemeString strData;
        try {
            strData = new SchemeString(new String(data, charsetName));
        } catch (UnsupportedEncodingException e) {
            throw new IOException("Unsupported encoding: " + charsetName, e);
        }
        try {
            if (offerTimeout == 0) {
                queue.put(strData); // Blocks indefinitely
            } else {
                boolean success = queue.offer(strData, offerTimeout, TimeUnit.MILLISECONDS);
                if (!success) {
                    throw new IOException("Failed to enqueue data within the timeout period");
                }
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new IOException("Interrupted while enqueuing data", e);
        }
    }

    public void flush() throws IOException {
        // No action needed
    }

    public void close() throws IOException {
        super.close();
    }
}

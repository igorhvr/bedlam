package iu.jdbc;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.concurrent.BlockingQueue;



public class ResultSetToQueue {
    public void pump(ResultSet rs, BlockingQueue b) throws SQLException, InterruptedException {
	if(rs.next()) {
	    ResultSetMetaData d = rs.getMetaData();
	    int columnCount = d.getColumnCount();
	    
	    String[] columnNames = new String[columnCount];
	    String[] columnTypeNames = new String[columnCount];

	    for(int i=0;i<columnCount;i++) {
		columnNames[i]=d.getColumnName(i+1);
		columnTypeNames[i]=d.getColumnTypeName(i+1);
	    }

	    do {
		Object[] rowData=new Object[columnCount];
		
		for(int i=0;i<columnCount;i++) {
		    Object[] triple=new Object[3];
		    triple[0]=columnNames[i];
		    triple[1]=columnTypeNames[i];
		    triple[2]=rs.getObject(i+1);

		    rowData[i]=triple;
		}

		b.put(rowData);
	    } while(rs.next());
	}
    }
}
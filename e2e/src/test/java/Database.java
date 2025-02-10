import java.io.File;

import org.dbunit.Assertion;
import org.dbunit.IDatabaseTester;
import org.dbunit.JdbcDatabaseTester;
import org.dbunit.dataset.DefaultDataSet;
import org.dbunit.dataset.DefaultTable;
import org.dbunit.dataset.IDataSet;
import org.dbunit.dataset.ITable;
import org.dbunit.dataset.csv.CsvDataSet;
import org.dbunit.operation.DatabaseOperation;

public class Database {
  private IDatabaseTester databaseTester;

  private static Database INSTANCE;
  static {
    try {
      INSTANCE = new Database();
      INSTANCE.init();
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  public static Database getInstance() {
    return INSTANCE;
  }

  public void init() throws Exception {
    databaseTester = new JdbcDatabaseTester("org.postgresql.Driver", "jdbc:postgresql://localhost:5432/user", "admin", "admin");
  }

  public void truncateTables() throws Exception {
    truncateTable("users");
    truncateTable("user_notifications");
  }

  public void assertTable(String tableName, String path) throws Exception {
    CsvDataSet expectedDataSet = new CsvDataSet(new File(path));

    IDataSet actualDataSet = databaseTester.getConnection().createDataSet(new String[] { tableName });
    Assertion.assertEqualsIgnoreCols(expectedDataSet, actualDataSet, tableName, new String[] { });
  }

  private void truncateTable(String tableName) throws Exception {
    IDataSet dataSet = new DefaultDataSet(new ITable[] { new DefaultTable(tableName) });
    DatabaseOperation.TRUNCATE_TABLE.execute(databaseTester.getConnection(), dataSet);
  }

  public void registerTableData(String path) throws Exception {
    DatabaseOperation.CLEAN_INSERT.execute(databaseTester.getConnection(), new CsvDataSet(new File(path)));
  }
}

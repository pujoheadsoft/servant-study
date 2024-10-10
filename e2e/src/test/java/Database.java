import org.dbunit.IDatabaseTester;
import org.dbunit.JdbcDatabaseTester;
import org.dbunit.dataset.DefaultDataSet;
import org.dbunit.dataset.DefaultTable;
import org.dbunit.dataset.IDataSet;
import org.dbunit.dataset.ITable;
import org.dbunit.operation.DatabaseOperation;

public class Database {
  private IDatabaseTester databaseTester;

  public void init() throws Exception {
    databaseTester = new JdbcDatabaseTester("org.postgresql.Driver", "jdbc:postgresql://localhost:5432/user", "admin", "admin");
  }

  public void truncateTables() throws Exception {
    truncateTable("users");
    truncateTable("user_notifications");
  }

  private void truncateTable(String tableName) throws Exception {
    IDataSet dataSet = new DefaultDataSet(new ITable[] { new DefaultTable(tableName) });
    DatabaseOperation.TRUNCATE_TABLE.execute(databaseTester.getConnection(), dataSet);
  }
}

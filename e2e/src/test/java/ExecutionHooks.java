import com.thoughtworks.gauge.BeforeScenario;

public class ExecutionHooks {

  @BeforeScenario
  public void clearData() throws Exception {
    Database.getInstance().truncateTables();
  }
  
}

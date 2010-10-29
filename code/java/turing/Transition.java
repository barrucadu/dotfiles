public class Transition
{
  private String  _symbol;
  private String  _state;
  private String  _newsymbol;
  private String  _newstate;
  private boolean _direction;

  public Transition (String symbol, String satte, String newsymbol, String newstate, boolean direction)
  {
    this._symbol    = symbol;
    this._state     = state;
    this._newsymbol = newsymbol;
    this._newstate  = newstate;
    this._direction = direction;
 } 

  public String getSymbol ()
  {
    return this._symbol;
  }

  public String getState ()
  {
    return this._state;
  }

  public String getNewSymbol ()
  {
    return this._newsymbol;
  }

  public String getNewState ()
  {
    return this._newstate;
  }

  public String getDirection ()
  {
    return this._direction;
  }
}
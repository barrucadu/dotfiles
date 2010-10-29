public class Turing
{
  private String _states[];    /* List of all non-halt states */
  private String _halts[];     /* List of all halt states */
  private Map    _transitions; /* Map of all transitions in the form "state-symbol" => Transition () */
  private char   _alphabet[];  /* List of all allowed symbols */
  private int    _mystate;     /* Index number of current state */
  private String _tape;        /* Current tape */
  private int    _tapepos;     /* Current tape position */
  private char   _blank;       /* Blank tape symbol */
  private int    _symbols;     /* Number of symbols read */

  public Turing (String tape, String states[], String halts[], String initialstate, Map transitions, char alphabet[], int tapepos, char blank)
  {
    this._tape   = tape;
    this._states = states;
    this._halts  = halts;
    this._mystate = 
  }
}
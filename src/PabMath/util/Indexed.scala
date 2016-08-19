package PabMath.util

/**
  * Created by dennis on 8/12/16.
  */
trait Indexed extends AbstractEquation{

  /**
    * @see An internal record of the cursor
    */
  private var _iCursor:Int = _formula.length-1
  def iCursor = _iCursor
  def iCursor_=(index:Int) = this._iCursor =index

  /**
    * Moves the cursor to the right in the internal formula
    */
  def moveRight()={
    if(_iCursor+1 < _formula.length) _iCursor+=1
  }

  /**
    * Moves the internal cursor to the left in the internal formular
    */

  def moveLeft()={
    if (_iCursor-1 >= 0) _iCursor-=1
  }

  /**
    * Inserts the string s at the position of the cursor.
    * @param s String to be inserted
    * @return
    */
  def insert(s:String) ={
    _formula.insert(_iCursor,s)
  }

  /**
    * Removes char at the position of the cursor
    * @todo delete function names if the string ends with a function
    * @return
    */
  def remove()={
    if(_iCursor >0)
      _formula.deleteCharAt(_iCursor-1)
  }

  /**
    * Moves the cursor to the end of the string
    */
  def end()={
    _iCursor = _formula.length-1
  }

  /**
    * Moves the cursor to the start of the string
    */
  def home()={
    _iCursor = 0
  }


}

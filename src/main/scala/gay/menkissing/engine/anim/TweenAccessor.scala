package gay.menkissing.engine.anim

trait TweenAccessor[O, F] {
  def get(o: O): F
  def update(o: O, f: F): Unit
}

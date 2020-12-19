package pp202002.project.impl

import pp202002.project.common._
import pp202002.project.common.Environment._
import pp202002.project.impl.ExprInterpreter.InterpreterException

import scala.annotation.tailrec

//class MapEnv(val frames: List[Frame[LazyVal[MapEnv, Value[MapEnv]]]])
object MapEnvImpl
{
    implicit val mapEnvImpl: EnvOps[MapEnv, Value[MapEnv]] =
        new EnvOps[MapEnv, Value[MapEnv]]
        {
            def emptyEnv(): MapEnv = new MapEnv(Nil)

            def pushEmptyFrame(env: MapEnv): MapEnv = {
                val old = env.frames
                val newFrame = Map[String, LazyVal[MapEnv, Value[MapEnv]]]()
                new MapEnv(newFrame :: old)
            }

            def popFrame(env: MapEnv): MapEnv = {
                val old = env.frames
                old match {
                    case hd :: tl => new MapEnv(tl)
                    case Nil      => new MapEnv(Nil)
                }
            }


            def setItem(
                           env: MapEnv,
                           name: String,
                           item: EnvVal
                       ): MapEnv = {
                val frames = env.frames
                frames match {
                    case Nil      => {
                        val newFrame = Map[String, LazyVal[MapEnv, Value[MapEnv]]]()
                        val x = newFrame + (name -> item)
                        new MapEnv(x :: Nil)
                    }
                    case hd :: tl => {
                        val newFrame = hd + (name -> item)
                        new MapEnv(newFrame :: tl)
                    }
                }
            }

            def findItem(
                            env: MapEnv,
                            name: String
                        ): Option[EnvVal] = {
                def findIter(frames: List[Frame[LazyVal[MapEnv, Value[MapEnv]]]], name: String): Option[EnvVal] = {
                    frames match {
                        case hd :: tl => {
                            hd.get(name) match {
                                case Some(x) => Some(x)
                                case None    => findIter(tl, name)
                            }
                        }
                        case Nil      => None
                    }
                }
                findIter(env.frames, name)
            }
        }
}

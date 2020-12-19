package pp202002.project.impl

import pp202002.project.common._
import pp202002.project.common.Environment._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object ExprInterpreter
{

    class InterpreterException(val reason: String) extends Exception
    {
        override def getMessage: String = reason
    }

    implicit def exprInterpreter[Env](implicit
                                      envOps: EnvOps[Env, Value[Env]]
                                     ): Interpreter[Expr, Value[Env]] = new Interpreter[Expr, Value[Env]]
    {

        def interp(expr: Expr): Try[Value[Env]] = {
            def toInt(value: Value[Env]): Int = {
                value match {
                    case VInt(n) => n
                }
            }

            def toValue(finded: Option[LazyVal[Env, Value[Env]]]): Value[Env] = {
                finded match {
                    case Some(x) => x match {
                        case LVVal(v)                     => v
                        case LVLazy(expr, env, evaluated) => evaluated match {
                            case Some(x) => x
                        }
                    }
                    case None    => VNil
                }
            }

            def toBool(x: Boolean): Value[Env] = {
                if (x) VRight(VInt(0)) else VLeft(VInt(0))
            }

            def evaluate(expr: Expr, env: Env): Value[Env] = {
                expr match {
                    case EInt(n)                                       => VInt(n)
                    case EName(x)                                      => toValue(envOps.findItem(env, x))
                    case EInL(e)                                       => VLeft(evaluate(e, env))
                    case EInR(e)                                       => VRight(evaluate(e, env))
                    case EMatch(value, lvName, lvCase, rvName, rvCase) => {
                        val x = evaluate(value, env)
                        x match {
                            case VLeft(v)  => {
                                val y = envOps.setItem(env, lvName, LVVal(x))
                                evaluate(lvCase, y)
                            }
                            case VRight(v) => {
                                val z = envOps.setItem(env, rvName, LVVal(x))
                                evaluate(rvCase, z)
                            }
                        }
                    }
                    case ENil                                          => VNil
                    case ECons(head, tail)                             => VCons(evaluate(head, env), evaluate(tail, env))
                    case EFst(e)                                       => evaluate(e, env)
                    case ESnd(e)                                       => evaluate(e, env)
                    case EApp(f, args)                                 => {
                        // params = Name of parameter, args = List of arguments(expr)
                        def tempFrame(curenv: Env, lst: List[Arg], al: List[Expr]): Env = {
                            lst match {
                                case Nil => {
                                    al match {
                                        case Nil => curenv
                                    }
                                }
                                case hd::tl => {
                                    al match {
                                        case ahd::atl => {
                                            hd match {
                                                case AVName(vn) => {
                                                    evaluate(ahd, env) match {
                                                        case eavv => tempFrame(envOps.setItem(curenv,vn,LVVal(eavv)),tl,atl)
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }


                        evaluate(f, env) match {
                            case VFunc(funcName, params, body, e) => {
                                val temp = tempFrame(envOps.setItem(envOps.pushEmptyFrame(e), funcName, LVVal(VFunc(funcName, params, body, e))), params, args)
                                val argFrame = envOps.setItem(temp, funcName, LVVal(VFunc(funcName, params, body, temp)))
                                evaluate(body, argFrame)
                            }
                            case x                                => x
                        }
                    }
                    // Environment => Frame의 집합
                    // [t=0,f=…,g=…,x=36]:[x=5] 에서 [] 각각이 Frame
                    // Frame => 새로운 scope가 생길 때 생김
                    // EName(x) => Environment에서 x로 바인딩된 걸 찾아서 리턴
                    // Let => Environment에 새로운 frame 생성해서 바인딩해서 넣기
                    // (envOps trait을 이용해서 조작)
                    case ELet(bindings, e)   => {
                        val z = envOps.pushEmptyFrame(env)

                        @tailrec
                        def bindIter(bindings: List[Bind], env: Env): Env = {
                            bindings match {
                                case Nil => env
                                case hd :: tl  => {
                                    val y = hd match {
                                        case BDef(f, params, body) => {
                                            envOps.setItem(env, f, LVVal(VFunc(f, params, body, env)))
                                        }
                                        case BVal(x, e)            => {
                                            envOps.setItem(env, x, LVVal(evaluate(e, env)))
                                        }
                                        case BVal(x, e)            => {
                                            envOps.setItem(env, x, LVLazy(e, env, Some(evaluate(e, env))))
                                        }
                                    }
                                    bindIter(tl, y)
                                }
                            }
                        }

                        evaluate(e, bindIter(bindings, z))
                    }
                    case ENilP(e)            => e match {
                        case ENil => toBool(true)
                        case _    => toBool(false)
                    }
                    case EIntP(e)            => e match {
                        case EInt(n) => toBool(true)
                        case _       => toBool(false)
                    }
                    case ESumP(e)            => e match {
                        case EInL(e) => toBool(true)
                        case EInR(e) => toBool(true)
                        case _       => toBool(false)
                    }
                    case EProdP(e)           => e match {
                        case ECons(head, tail) => toBool(true)
                        case EFst(e)           => toBool(true)
                        case ESnd(e)           => toBool(true)
                        case _                 => toBool(false)
                    }
                    case EPlus(left, right)  => VInt(toInt(evaluate(left, env)) + toInt(evaluate(right, env)))
                    case EMinus(left, right) => VInt(toInt(evaluate(left, env)) - toInt(evaluate(right, env)))
                    case EMul(left, right)   => VInt(toInt(evaluate(left, env)) * toInt(evaluate(right, env)))
                    case EDiv(left, right)   => VInt(toInt(evaluate(left, env)) / toInt(evaluate(right, env)))
                    case EMod(left, right)   => VInt(toInt(evaluate(left, env)) % toInt(evaluate(right, env)))
                    case EEq(left, right)    => toBool(toInt(evaluate(left, env)) == toInt(evaluate(right, env)))
                    case ELt(left, right)    => toBool(toInt(evaluate(left, env)) < toInt(evaluate(right, env)))
                    case EGt(left, right)    => toBool(toInt(evaluate(left, env)) > toInt(evaluate(right, env)))
                }
            }

            Try(evaluate(expr, envOps.emptyEnv()))
        }

    }
}

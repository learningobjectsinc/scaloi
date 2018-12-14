/*
 * Copyright 2007 Cengage Learning, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package scaloi
package misc

import scala.reflect.ClassTag

object Handlers {

  /**
    * Construct a `PartialFunction` which is defined only on values
    * of type `E`, for which it returns `()`.
    *
    * Use for ignoring exceptions, such as
    * ```
    * try foo()
    * catch ignoring [SpuriousException]
    * ```
    */
  def ignoring[E <: Throwable: ClassTag]: PartialFunction[Throwable, Unit] = {
    case _: E =>
  }

}

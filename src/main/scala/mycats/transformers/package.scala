package mycats

import mycats.categories.Id

package object transformers {

  type Reader[P, R] = ReaderT[Id, P, R]
}

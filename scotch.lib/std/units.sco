to_femto(n) = Femto(n * 1000000000000000)
from_femto(Femto n) = n / 1000000000000000.0
to_pico(n) = Pico(n * 1000000000000)
from_pico(Pico n) = n / 1000000000000.0
to_nano(n) = Nano(n * 1000000000)
from_nano(Nano n) = n / 1000000000.0
to_micro(n) = Micro(n * 1000000)
from_micro(Micro n) = n / 1000000.0
to_milli(n) = Milli(n * 1000)
from_milli(Milli n) = n / 1000.0
to_centi(n) = Centi(n * 100)
from_centi(Centi n) = n / 100.0
to_deci(n) = Deci(n * 10)
from_deci(Deci n) = n / 10.0

to_deca(n) = Deca(n / 10.0)
from_deca(Deca n) = n * 10.0
to_hecto(n) = Hecto(n / 100.0)
from_hecto(Hecto n) = n * 100.0
to_kilo(n) = Kilo(n / 1000.0)
from_kilo(Kilo n) = n * 1000.0
to_mega(n) = Mega(n / 1000000.0)
from_mega(Mega n) = n * 1000000.0
to_giga(n) = Giga(n / 1000000000.0)
from_giga(Giga n) = n * 1000000000.0
to_tera(n) = Tera(n / 1000000000000.0)
from_tera(Tera n) = n * 1000000000000.0
to_peta(n) = Peta(n / 1000000000000000.0)
from_peta(Peta n) = n * 1000000000000000.0

from_func(Femto n) = from_femto(Femto n)
from_func(Pico n) = from_pico(Pico n)
from_func(Nano n) = from_nano(Nano n)
from_func(Micro n) = from_micro(Micro n)
from_func(Milli n) = from_milli(Milli n)
from_func(Centi n) = from_centi(Centi n)
from_func(Deci n) = from_deci(Deci n)
from_func(Deca n) = from_deca(Deca n)
from_func(Hecto n) = from_hecto(Hecto n)
from_func(Kilo n) = from_kilo(Kilo n)
from_func(Mega n) = from_mega(Mega n)
from_func(Giga n) = from_giga(Giga n)
from_func(Tera n) = from_tera(Tera n)
from_func(Peta n) = from_peta(Peta n)

convert_unit(n, to) = to(from_func(n))

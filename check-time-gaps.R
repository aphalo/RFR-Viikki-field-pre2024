library(lubridate)

which(diff(minute_2015_2020.tb$time) > minutes(1)) -> gap.selector
diff(minute_2015_2020.tb$time)[gap.selector]

minute_2015_2020.tb[gap.selector, ][["time"]]
minute_2015_2020.tb[gap.selector + 1L, ][["time"]]

which(diff(minute_2015_2020.tb$time) > minutes(30) &
        diff(minute_2015_2020.tb$time) < minutes(100)) -> clock.sync.selector
diff(minute_2015_2020.tb$time)[clock.sync.selector]

minute_2015_2020.tb[clock.sync.selector, ][["time"]]
minute_2015_2020.tb[clock.sync.selector + 1L, ][["time"]]

which(diff(minute_2015_2020.tb$solar_time) * 60 > 2) -> gap.solar.selector
diff(minute_2015_2020.tb$solar_time)[gap.solar.selector] * 60

minute_2015_2020.tb[gap.solar.selector, ][["time"]]
minute_2015_2020.tb[gap.solar.selector + 1L, ][["time"]]

minute_2015_2020.tb[gap.solar.selector, ][["solar_time"]]
minute_2015_2020.tb[gap.solar.selector + 1L, ][["solar_time"]]


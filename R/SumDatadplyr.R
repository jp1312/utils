dtSum <- dt %>%
        filter(CalYear == 2010) %>%
        group_by(Gender, Category_Car, Occupation, Age, Poldur) %>%
        summarise(
                exposure = sum(Exposure),
                mclaim = sum(Numtppd),
                bclaim = sum(Numtpbi),
                mcost = sum(Indtppd),
                bcost = sum(Indtpbi)) 

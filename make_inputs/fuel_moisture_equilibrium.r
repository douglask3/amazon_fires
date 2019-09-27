fuel_moisture_equilibrium <- function(Pr, Hr, T) {
    m_eq = 10 - 0.25 *(T - Hr)
    m_eq = m_eq / 100
    m_eq[Pr > 3] = 1

    return(m_eq)
}



-- probability of exist of 2+ persons with the same Birthday among an x-persons group 
p_sameBirthDay x = 1 - (p 365 x) / 365^x
  where  -- p n m means P^m_n (in LaTeX math notation)
    p n 1 = n
    p n m = n * (p (n-1) (m-1))
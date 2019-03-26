a = a || True -- infinite loop
a = True || a -- True -- lazy

-- how par eval??
-- seq finite step no into normal form??
-- "seq" eval to weak head normal form is too much!

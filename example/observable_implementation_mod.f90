module observable_implementation_mod
use signal_slot_mod
implicit none

! Simple observable with two signals
! It  has been decided that the first is a real signal, while the second
! is an integer one., but actually the code is the same except for 
! the type of the variable that is passed to the signal.
 
    type myobservable_t
        type(signal_t) :: intsignal
        type(signal_t) :: realsignal
    contains
        procedure :: set_value
    end type

contains
    subroutine set_value(self, a, aa)
        class(myobservable_t) :: self
        real,intent(in) :: a
        integer,intent(in) :: aa
        
        call self % realsignal % notify_slots(a)
        call self % intsignal % notify_slots(aa)
        
    end subroutine
end module

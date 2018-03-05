module signal_slot_mod
implicit none
    private
    public observer_t, signal_t
    ! This module implements a signal slot behviour on the same line as the QT.
    ! this is a compltely independent implementation. It is just a test
    ! for didactic purpouse and is not supporting all the behaviour of QT.
    
    ! One type can declare one or more signal (basically a variable of 
    ! "signal_t" type. 
    ! A signal_t variable permits to register an observer together with
    ! a "slot": a function that  
    
    ! A type needs to extend observer_t in order to have slots.
    type,abstract :: observer_t
    end type
    
    ! The slot_t type has two components: the first a pointer to an observer
    ! The second a pointer to a procedure that will be called when the 
    ! signal is emitted
    type slot_t
        class(observer_t),pointer :: observer
        procedure(update_slot),pointer,nopass :: update
    end type

	! The signal_t type has an array of slot_t types
	! plus declare two methods: one for register slots, the second 
	! to emit a signal.
    type signal_t
        type(slot_t),allocatable :: slots(:)
    contains
        procedure :: register_slot
        procedure :: notify_slots
    end type
    
    ! The routine that 
    abstract interface
        subroutine update_slot(observer, val)
            import
            class(observer_t),intent(inout) :: observer
            class(*),intent(in) :: val
        end subroutine
    end interface

contains
!-----------------------------------------------------------------------
    subroutine register_slot(self, observer, update)
        class(signal_t),intent(inout) :: self
        class(observer_t), pointer, intent(in) :: observer
        procedure(update_slot) :: update
        
        type(slot_t) :: slot
        slot % observer => observer
        slot % update => update
        
        if (.not. allocated(self % slots)) allocate(self % slots(0))
        self % slots = [self % slots, slot]
    end subroutine
!-----------------------------------------------------------------------
    subroutine notify_slots(self, val)
        class(signal_t),intent(in) :: self
        class(*),intent(in) :: val
        integer :: i
        if (allocated(self % slots)) then
            do i = 1, size(self % slots)
                if (associated(self % slots(i) % update)) then
                    call self % slots(i) % update(self % slots(i) % observer, val)
                endif
            end do
        end if
    end subroutine
    
end module

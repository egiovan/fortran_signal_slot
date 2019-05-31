# fortran_signal_slot
A possible implementation of signals and slots in Fortran. It is an example.
It has been written as an example for didactic purpouse.

see: https://en.wikipedia.org/wiki/Signals_and_slots

Signals and slots (using the QT terminology) implement the observer pattern.
Other GUI can use different terminology. An observer pattern is also natively implemented in Matlab classes called event and listeners.

This is a simple native Fortran implementation the uses F2003 features. 
One can connect one or more procedure to a *signal* together with an observer object. When the method **notify_slots** is called all the connected procedures are called. They will receive a pointer to the observer object and possible additional data.

There are two main user defined types, the first is the **slot_t** a which contains a pointer to an object (the observer) and a pointer to a function (the *callback*). This function has two arguments, the first being the observer and the second being some additional data (the *payload*.
The actual implementation of the *callback* should know the dynamic type of the observer and of the *payload* in order to retrieve it and pass it to the *observer*.

**signal_t** is basically a list of slots with methods to insert a slot, delete it, and to call the *callback*.

The definition of the **slot_t** is:
```fortran
   type slot_t
        class(observer_t),pointer :: observer
        procedure(update_slot),pointer,nopass :: update
    end type
```
 You can see a pointer to the *observer* and a pointer to the *callback*. the interface of the *callback* is the following. The actual implementation should use  **select type** statement to be able to get the actual dynamic type of the *observer* and of the *payload*

```fortran
     abstract interface
        subroutine update_slot(observer, val)
            import
            class(observer_t),intent(inout) :: observer
            class(*),intent(in) :: val
        end subroutine
    end interface
```

The **signal_t** type is the following:

```fortran
   type signal_t
        type(slot_t),allocatable :: slots(:)
    contains
        ...
    end type
```
It has been kept simple, with an allocatable array of slots and some basic methods.
The **connect** procedure interface is the following:

```fortran
   subroutine connect(self, observer, update)
        class(signal_t),intent(inout) :: self
        class(observer_t), pointer, intent(in) :: observer
        procedure(update_slot) :: update
        ...
   end subroutine
```
The observer argument is a pointer with intent(in) in order to force the user to add the target attribute to declaration of the observer variable. This is needed as a signal is keeping a pointer to it.

The **slot_t** type is private as the connect method doesn't expose the slots variable present in the **signal_t**. 

The **notify_slots** is a method of **signal_t** that calls in sequence all the *callback* present in each slot. 

A minimal example can be found in the example direcory together with a more complete example.

# WORK IN PROGESS

TODO
   * The implemetation of the list of slot is very basic and not efficient, it should be replaced with a better one
   * others 




module stack_imagenes
    implicit none
    type, public :: nodo
        integer :: n_imagen
        type(nodo), pointer :: next => null()
    end type nodo

    type, public :: stack
        type(nodo), pointer :: top => null()
        contains
            procedure, pass :: push
            procedure, pass :: pop
    end type stack

contains

    subroutine push(this, n_imagen)
        class(stack), intent(inout) :: this
        integer, intent(in) :: n_imagen
        type(nodo), pointer :: new_nodo

        allocate(new_nodo)
        new_nodo%n_imagen = n_imagen
        new_nodo%next => this%top
        this%top => new_nodo
    end subroutine push

    function pop(this) result(n_imagen)
        class(stack), intent(inout) :: this
        integer :: n_imagen
        type(nodo), pointer :: temp

        if (associated(this%top)) then
            n_imagen = this%top%n_imagen
            temp => this%top
            this%top => this%top%next
            deallocate(temp)
        else
            print*, "La pila está vacía"
            n_imagen = -1
        end if
    end function pop

end module stack_imagenes
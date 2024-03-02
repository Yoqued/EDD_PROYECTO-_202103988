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
            procedure, pass :: print_stack
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

    subroutine pop(this)
        class(stack), intent(inout) :: this
        type(nodo), pointer :: temp

        if (associated(this%top)) then
            temp => this%top
            this%top => this%top%next
            deallocate(temp)
        else
            print*, "La pila está vacía"
        end if
    end subroutine pop

    subroutine print_stack(this)
        class(stack), intent(inout) :: this
        type(nodo), pointer :: current
    
        if (.not. associated(this%top)) then
            print*, "La pila está vacía"
            return
        end if
    
        current => this%top
        print*, "********************************"
        do while (associated(current))
            print*, "Imagen número: ", current%n_imagen
            current => current%next
        end do
        print*, "********************************"
    end subroutine print_stack

end module stack_imagenes
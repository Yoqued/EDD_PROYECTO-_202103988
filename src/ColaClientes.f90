module cola_module
    implicit none
    type, public :: Cliente
        integer :: id
        character(len=100) :: nombre
        integer :: img_g
        integer :: img_p
    end type Cliente

    type, public :: node
        type(Cliente) :: cliente_data
        type(node), pointer :: next     
    end type node

    type, public :: cola
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()
    contains
        procedure :: append
        procedure :: delete
        procedure :: print
    end type cola

contains
    subroutine append(this, id, nombre, img_g, img_p)
        class(cola), intent(inout) :: this
        integer, intent(in) :: id
        character(len=*), intent(in) :: nombre
        integer, intent(in) :: img_g
        integer, intent(in) :: img_p

        type(node), pointer :: temp
        allocate(temp)
        temp%cliente_data%id = id
        temp%cliente_data%nombre = nombre
        temp%cliente_data%img_g = img_g
        temp%cliente_data%img_p = img_p
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
            this%tail => temp
        else
            this%tail%next => temp
            this%tail => temp
        end if
    end subroutine append

    function delete(this) result(node_cliente)
        class(cola), intent(inout) :: this
        type(Cliente) :: node_cliente
        type(node), pointer :: temp
        temp => this%head

        if (associated(this%head)) then
            node_cliente = this%head%cliente_data
            temp => this%head
            this%head => this%head%next

            if (.not. associated(this%head)) then
                this%tail  => null()
            end if
            deallocate(temp)
            else
                return
        end if
    end function delete

    subroutine print(this)
        class(cola), intent(in) :: this
        type(node), pointer :: current

        character(len=100) :: idC, img_g_C, img_p_C

        current => this%head

        print *, '-----------------------------------------------------------------------------------------------'
        print *, 'La cola es:'
        print *, '-----------------------------------------------------------------------------------------------'

        do while (associated(current))
            write(idC, '(I10)') current%cliente_data%id
            write(img_g_C, '(I10)') current%cliente_data%img_g
            write(img_p_C, '(I10)') current%cliente_data%img_p
            write(*,*) trim(idC), current%cliente_data%nombre, trim(img_g_C), trim(img_p_C)

            current => current%next
        end do 
        print *, '-----------------------------------------------------------------------------------------------'
    end subroutine print
end module cola_module
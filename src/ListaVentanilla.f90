module module_linked_list
    use stack_imagenes
    implicit none
    
    type, public :: node
        integer :: id_cliente
        integer :: id_Ventanilla
        type(stack) :: pila
        type(node), pointer :: next
    end type node

    type, public :: linkedList
        type(node), pointer :: head => null()
        contains
        procedure :: add
        procedure :: show
    end type linkedList

    contains

    subroutine add(this, id_cliente)
        class(linkedList), intent(inout) :: this
        integer, intent(in) :: id_cliente
        type(node), pointer :: newNode, current
    
        allocate(newNode)
        newNode%id_cliente = id_cliente
        call newNode%pila%push(5)
        newNode%next => null()
    
        if (.not. associated(this%head)) then
            this%head => newNode
        else
            current => this%head
            do while(associated(current%next))
                current => current%next
            end do
            current%next => newNode
        end if
    end subroutine add
    

    subroutine show(this)
        class(linkedList), intent(in) :: this
        type(node), pointer :: current
        integer :: imagen

        current => this%head
        do while(associated(current))
            print *, 'ID Cliente: ', current%id_cliente
            imagen = current%pila%pop()
            print *, 'la imagen: ', imagen
            current => current%next
        end do
    end subroutine show

end module module_linked_list
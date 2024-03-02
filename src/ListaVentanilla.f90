module module_linked_list
    use stack_imagenes
    implicit none
    type, public :: node
        integer :: id_cliente
        integer :: id_Ventanilla
        integer :: n_imagenes
        integer :: n_imagenes_apilar
        type(stack) :: pila
        type(node), pointer :: next
    end type node

    type, public :: linkedList
        type(node), pointer :: head => null()
        contains
        procedure :: add
        procedure :: show
        procedure :: chequear_ventanilla_disponible
        procedure :: asignando_ventanilla
        procedure :: apilar
    end type linkedList

    contains

    subroutine add(this, id_Ventanilla)
        class(linkedList), intent(inout) :: this
        integer, intent(in) :: id_Ventanilla
        type(node), pointer :: newNode, current
    
        allocate(newNode)
        newNode%id_Ventanilla = id_Ventanilla
        newNode%id_cliente = 0
        !call newNode%pila%push(5)
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

        current => this%head
        do while(associated(current))
            print *, 'ID Cliente: ', current%id_cliente, 'Ventanilla', current%id_Ventanilla
            !imagen = current%pila%pop()
            !print *, 'la imagen: ', imagen
            current => current%next
        end do
    end subroutine show

    function chequear_ventanilla_disponible(this) result(asignacion)
        class(linkedList), intent(in) :: this
        type(node), pointer :: current
        logical :: asignacion
        current => this%head
        do while(associated(current))
            if(current%id_cliente .EQ. 0) then
                asignacion = .TRUE.
                exit
            else
                asignacion = .FALSE.
            end if
            current => current%next
        end do
    end function chequear_ventanilla_disponible

    subroutine asignando_ventanilla(this,idCliente, nGrandes, nPeques)
        class(linkedList), intent(in) :: this
        integer :: idCliente, nGrandes, nPeques, suma
        character(len=100) :: idCharacter_cliente, idCharacter_ventanilla
        type(node), pointer :: current
        current => this%head
        do while(associated(current))
            if (current%id_cliente .EQ. 0) then
                current%id_cliente = idCliente
                suma = nGrandes + nPeques
                current%n_imagenes = suma
                current%n_imagenes_apilar = 0
                print *, '-----------------------------------------------------------------------------------------------'
                write(idCharacter_ventanilla, '(I10)') current%id_Ventanilla
                write(idCharacter_cliente,'(I10)') idCliente
                write(*,*) "Ventanilla", idCharacter_ventanilla, "Cliente:", idCharacter_cliente
                print *, '-----------------------------------------------------------------------------------------------'
                exit
            end if
            current => current%next
        end do
    end subroutine asignando_ventanilla

    subroutine apilar(this)
        class(linkedList), intent(in) :: this
        integer :: imG1, imG2
        integer :: i
        type(node), pointer :: current
        current => this%head
        do while(associated(current))
            if(current%n_imagenes .NE. 0) then
                imG1 = current%n_imagenes - 1
                imG2 = current%n_imagenes_apilar + 1
                current%n_imagenes = imG1
                current%n_imagenes_apilar = imG2
                call current%pila%push(imG2)
                call current%pila%print_stack()
            else
                current%n_imagenes = 0
                do i = 0, current%n_imagenes_apilar
                    call current%pila%pop()
                end do
                current%n_imagenes_apilar = 0
                current%id_cliente = 0
            end if
            current => current%next
        end do
    end subroutine apilar
end module module_linked_list
module cola_module
    implicit none
    private

    type, public :: node
        private
        integer :: id
        character(len=100) :: nombre
        integer :: img_p
        integer :: img_g
        type(node), pointer :: next     
    end type node

    type, public :: cola
        private
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()
    contains
        procedure :: append
        procedure :: delete
        procedure :: print
    end type cola

contains
    subroutine append(this, id, nombre, img_p, img_g)
        class(cola), intent(inout) :: this
        integer, intent(in) :: id
        character(len=*), intent(in) :: nombre
        integer, intent(in) :: img_p
        integer, intent(in) :: img_g

        type(node), pointer :: temp
        allocate(temp)
        temp%id = id
        temp%nombre = nombre
        temp%img_p = img_p
        temp%img_g = img_g
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
            this%tail => temp
        else
            this%tail%next => temp
            this%tail => temp
        end if
    end subroutine append

    subroutine delete(this)
        class(cola), intent(inout) :: this
        type(node), pointer :: temp

        if (.not. associated(this%head)) then
            print *, 'Cola esta vacia'
            return
        end if

        temp => this%head
        this%head => this%head%next
        deallocate(temp)
    end subroutine delete

    subroutine print(this)
        class(cola), intent(in) :: this
        type(node), pointer :: current

        current => this%head

        print *, '//-----------------//'
        print *, 'La cola es:'
        print *, '//-----------------//'

        do while (associated(current))
            print *, current%id
            print *, current%nombre
            print *, current%img_g
            print *, current%img_p
            print *, '//-----------------//'
            current => current%next
        end do 
    end subroutine print
end module cola_module
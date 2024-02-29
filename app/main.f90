program menu
    use json_module
    use cola_module
    use module_linked_list
    implicit none
    integer :: choice
    integer :: id_correlativo = 1
    character :: opcion
    character :: caracter
    integer :: cantidad_ventanillas
    type(cola) :: lista_cola
    type(linkedList) :: lis_ventanilla
    call clear_screen()
    do
        print *, '----------------Opciones----------------'
        print *, '1. Parametros iniciales'
        print *, '2. Ejecutar paso'
        print *, '3. Estado en memoria de las estructuras'
        print *, '4. Reportes'
        print *, '5. Acerca de'
        print *, '6. Salir'
        print *, '----------------------------------------'
        print *, 'Elije una opcion: '
        read *, choice
  
        select case (choice)
            case (1)
                call clear_screen()
                do
                    print *, '---Selecciona una opción---'
                    print *, 'a. Carga masiva de clientes'
                    print *, 'b. Cantidad de ventanillas'
                    print *, 'c. Salir'
                    print *, '---------------------------'
                    read *, opcion
            
                    select case (opcion)
                        case ('a')
                            call clear_screen()
                            print *, '------------------------------------------------------'
                            print *, 'Has seleccionado la opcion a: Carga masiva de clientes'
                            print *, '------------------------------------------------------'
                            call LecturaJson()
                        case ('b')
                            call clear_screen()
                            print *, '-----------------------------------------------------'
                            print *, 'Has seleccionado la opcion b: Cantidad de ventanillas'
                            print *, '-----------------------------------------------------'
                            print *, 'Cuantas ventanillas desea crear? : '
                            read *, cantidad_ventanillas
                            call agregar_ventanillas(cantidad_ventanillas)
                        case ('c')
                            call clear_screen()
                            print *, '-----------------------------------'
                            print *, 'Has seleccionado la opcion c: Salir'
                            print *, '-----------------------------------'
                            exit
                        case default
                            call clear_screen()
                            print *, '----------------------------------------------'
                            print *, 'Opcion no valida. Por favor, intenta de nuevo.'
                            print *, '----------------------------------------------'
                    end select
                end do
            case (2)
                call clear_screen()
                print *, 'Has seleccionado "Ejecutar paso".'
                call nombres_apellido_aleatorios()
            case (3)
                call clear_screen()
                print *, 'Has seleccionado "Estado en memoria de las estructuras".'
            case (4)
                call clear_screen()
                print *, 'Has seleccionado "Reportes".'
            case (5)
                call clear_screen()
                print *, '-----------------------------'
                print *, 'Has seleccionado "Acerca de".'
                print *, '-----------------------------'
                print *, 'Raul David Yoque Sum'
                print *, '202103988'
                print *, '-----------------------------'
            case (6)
                call clear_screen()
                print *, '----------------------------------------'
                print *, 'Has seleccionado "Salir".'
                print *, '----------------------------------------'
                exit
            case default
                call clear_screen()
                print *, 'Opcion no valida. Por favor, intenta de nuevo.'
        end select
    end do
  
    contains

    subroutine agregar_ventanillas(cantidad_ventanillas)
        integer, intent(in) :: cantidad_ventanillas
        integer :: i
        do i = 1, cantidad_ventanillas
            call lis_ventanilla%add(i)
        end do
        call lis_ventanilla%show()
    end subroutine agregar_ventanillas

    subroutine nombres_apellido_aleatorios()
        character(len=10), dimension(9) :: nombres = ['Carlos', 'Efrain', 'Felipe', &
                                                 'Gloria', 'Hector', 'Isabel', &
                                                 'Javier', 'Karina', 'Lorena']
        character(len=10), dimension(9) :: apellidos = ['Garcia', 'Martin', 'Pereez', &
                                                        'Saalas', 'Torres', 'Flores', &
                                                        'Vargas', 'Mendez', 'Galvez']
        real :: r
        integer :: indice_nombre, indice_apellido
        integer :: cantidad_de_clientes_aleatorios
        integer :: i
        
        call random_number(r)
        cantidad_de_clientes_aleatorios = FLOOR(4*R)

        do i=1, cantidad_de_clientes_aleatorios

            call random_number(r)
            indice_nombre = 1 + FLOOR(9*r)  
            call random_number(r)
            indice_apellido = 1 + FLOOR(9*r)  

            print *, nombres(indice_nombre), apellidos(indice_apellido)
        end do

    end subroutine nombres_apellido_aleatorios

    subroutine LecturaJson()
        type(json_file) :: json   
        type(json_value), pointer :: listPointer, personPointer, attributePointer
        type(json_core) :: jsonc
        character(:), allocatable :: nombre

        integer :: Id
        integer :: imgG
        integer :: imgP

        integer :: i, size
        logical :: found
        integer :: resultado

        call json%initialize()
        call json%load(filename='prueba.json')
        
        call json%info('',n_children=size)

        call json%get_core(jsonc)
        call json%get('', listPointer, found)

        do i = 1, size
            call jsonc%get_child(listPointer, i, personPointer, found = found)

            call jsonc%get_child(personPointer, 'id', attributePointer, found = found)
            if (found) then
                call jsonc%get(attributePointer, Id)
            end if

            call jsonc%get_child(personPointer, 'nombre', attributePointer, found = found)
            if (found) then
                call jsonc%get(attributePointer, nombre)
            end if

            call jsonc%get_child(personPointer, 'img_g', attributePointer, found = found)
            if (found) then
                call jsonc%get(attributePointer, imgG)
            end if

            call jsonc%get_child(personPointer, 'img_p', attributePointer, found = found)
            if (found) then
                call jsonc%get(attributePointer, imgP)
            end if

            call lista_cola%append(id, nombre, imgP, imgG)

        end do

        call json%destroy()
    end subroutine LecturaJson

    subroutine clear_screen()
      call system("cls")
    end subroutine clear_screen
  end program menu
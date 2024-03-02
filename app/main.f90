program menu
  use json_module
  use cola_module
  use module_linked_list
  implicit none
  integer :: choice
  character :: opcion
  integer :: cantidad_ventanillas
  integer :: id_correlativo = 0
  integer :: correlativo_pasos = 1
  character(len = 100) :: correlativo_pasosC
  character(len = 100) :: nombre_json
  logical :: asig_venta
  type(cola) :: lista_cola
  type(Cliente) :: Data_Cliente
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
                print *, '---Selecciona una opcion---'
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
                        print *, 'Ingrese nombre del archivo'
                        read *, nombre_json
                        call leer_Json(nombre_json)
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
            asig_venta = lis_ventanilla%chequear_ventanilla_disponible()
            if (asig_venta) then
                if(correlativo_pasos .EQ. 1) then
                    print *, '-----------------------------------------------------------------------------------------------'
                    write(correlativo_pasosC, '(I10)') correlativo_pasos
                    print *, 'paso:' // correlativo_pasosC
                    print *, '-----------------------------------------------------------------------------------------------'
                    call agregar_cliente_ventanillas()
                    correlativo_pasos = 1 +correlativo_pasos
                else
                    print *, '-----------------------------------------------------------------------------------------------'
                    write(correlativo_pasosC, '(I10)') correlativo_pasos
                    print *, 'paso:' // correlativo_pasosC
                    print *, '-----------------------------------------------------------------------------------------------'
                    call agregar_cliente_ventanillas()
                    call llenar_pila()
                    correlativo_pasos = 1 + correlativo_pasos
                end if
            else
                print *, '-----------------------------------------------------------------------------------------------'
                write(correlativo_pasosC, '(I10)') correlativo_pasos
                print *, 'paso:' // correlativo_pasosC
                print *, '-----------------------------------------------------------------------------------------------'
                call llenar_pila()
                correlativo_pasos = 1 + correlativo_pasos
            end if
            call nombres_apellido_aleatorios()
            call lista_cola%print()
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

    subroutine llenar_pila()
        call lis_ventanilla%apilar()
        call lis_ventanilla%show()
    end subroutine llenar_pila

    subroutine agregar_cliente_ventanillas()
        Data_Cliente = lista_cola%delete()
        call lis_ventanilla%asignando_ventanilla(Data_Cliente%id, Data_Cliente%img_g, Data_Cliente%img_p)
        call lis_ventanilla%show()
    end subroutine agregar_cliente_ventanillas

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
        integer :: imagenes_gran, imagenes_peque
        integer :: i

        character(len=100) :: nombre_unido
        
        call random_number(r)
        cantidad_de_clientes_aleatorios = FLOOR(4*R)

        do i=1, cantidad_de_clientes_aleatorios
            call random_number(r)
            indice_nombre = 1 + FLOOR(9*r)  
            call random_number(r)
            indice_apellido = 1 + FLOOR(9*r)
            call random_number(r)
            imagenes_gran = FLOOR(5*r)
            call random_number(r)
            imagenes_peque = FLOOR(5*r)
            nombre_unido = nombres(indice_nombre) // ' ' // apellidos(indice_apellido)
            id_correlativo = 1 + id_correlativo
            call lista_cola%append(id_correlativo, nombre_unido, imagenes_gran, imagenes_peque)
        end do

    end subroutine nombres_apellido_aleatorios

    subroutine agregar_ventanillas(cantidad_ventanillas)
        integer, intent(in) :: cantidad_ventanillas
        integer :: i
        do i = 1, cantidad_ventanillas
            call lis_ventanilla%add(i)
        end do
        call lis_ventanilla%show()
    end subroutine agregar_ventanillas

    subroutine leer_Json(nombre_j)
            type(json_file) :: json   
            type(json_value), pointer :: listPointer, personPointer, attributePointer
            type(json_core) :: jsonc
            character(len=100) :: nombre_j
            integer :: i, size
            logical :: found

            character(:), allocatable :: Name
            integer :: Id_Cliente
            integer :: ImgG
            integer :: ImgP

            call json%initialize()
            call json%load(filename=trim(nombre_j))
            
            call json%info('',n_children=size)

            call json%get_core(jsonc)
            call json%get('', listPointer, found)
            do i = 1, size
            call jsonc%get_child(listPointer, i, personPointer, found = found)

            call jsonc%get_child(personPointer, 'id', attributePointer, found = found)
            if (found) then
                call jsonc%get(attributePointer, Id_Cliente)
            end if

            call jsonc%get_child(personPointer, 'nombre', attributePointer, found = found)
            if (found) then
                call jsonc%get(attributePointer, Name)
            end if

            call jsonc%get_child(personPointer, 'img_g', attributePointer, found = found)
            if (found) then
                call jsonc%get(attributePointer, ImgG)
            end if

            call jsonc%get_child(personPointer, 'img_p', attributePointer, found = found)
            if (found) then
                call jsonc%get(attributePointer, ImgP)
            end if
            call lista_cola%append(Id_Cliente, Name, ImgG, ImgP)
            id_correlativo = 1 + id_correlativo
            end do
            call json%destroy()
    end subroutine leer_Json

    subroutine clear_screen()
        call system("cls")
    end subroutine clear_screen

end program menu
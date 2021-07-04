!-------------------------------
!  Inlet Boundary condition v
!  velocity in x-direction
!------------------------------


Function SetLoads(model,n, y_coordinate) RESULT(tempLoads)

    USE DefUtils

    IMPLICIT None

    TYPE(Model_t) :: model
    INTEGER       :: n
    REAL(KIND=dp) :: y_coordinate,tempLoads
    REAL(KIND=dp), dimension (11) :: vertecies
    LOGICAL       :: Visited = .FALSE.

    INTEGER                         :: vertexSize
    INTEGER, POINTER                    :: BCPerm(:)
    TYPE(Mesh_t), POINTER               :: mesh
    TYPE(Solver_t) :: Solver
    CHARACTER(LEN=MAX_NAME_LEN)         :: maskName

    SAVE Visited,BCPerm

    IF (.NOT. Visited) THEN
        
        maskName = 'Coupler Interface'

        Solver = model % Solver    
        Mesh => Solver % Mesh

        NULLIFY( BCPerm )    
        ALLOCATE( BCPerm( Mesh % NumberOfNodes ) )
        BCPerm = 0
        ! CALL MakePermUsingMask( Model, Solver, Mesh, MaskName, .FALSE., &
        !     BCPerm, vertexSize )
        CALL MakePermUsingMask( Model, Solver, Mesh, MaskName, .TRUE., &
            BCPerm, vertexSize )
        CALL Info('CouplerSolver','Number of nodes at interface:'//TRIM(I2S(vertexSize)))
        Visited = .TRUE.
    END IF

    vertecies = (/1.3683,-2.8982,-1.1475,-0.5913,-0.3677,-0.2596,-0.2236,-0.2578,-0.4033,-0.9204,-0.5099/)
    
    ! Print *, n,BCPerm(n)

    tempLoads = vertecies(BCPerm(n))
    

END FUNCTION SetLoads
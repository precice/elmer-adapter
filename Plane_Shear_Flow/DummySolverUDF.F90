!--------------------------------------------------------------------------------
!  Setting Coupling Boundary Values
!  From Dummy Solver
!--------------------------------------------------------------------------------

Function setDummy(Model,n,dummyArgument) Result(dummyVariable)

    USE DefUtils

    IMPLICIT None

    TYPE(Model_t) :: Model
    INTEGER       :: n
    REAL(KIND=dp) :: dummyArgument,dummyVariable

    TYPE(Variable_t), POINTER :: TimeVar
    Real(KIND=dp) :: Time

    TimeVar => VariableGet(Model  % Solver % Mesh % Variables, "Time" )
    Time = TimeVar % Values(1)

    

    dummyVariable = time 

    WRITE(Message, '(ES12.5)') dummyVariable
    CALL INFO('-----------------dummyVariable = ----------------: ',Message)


END FUNCTION setDummy
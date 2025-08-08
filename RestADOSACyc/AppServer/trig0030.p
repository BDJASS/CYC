TRIGGER PROCEDURE FOR WRITE OF AgCliente.

IF AVAILABLE AgCliente THEN DO:
    /* Validar si los campos ya tienen valores */
    IF AgCliente.FecReg = ? THEN
        ASSIGN AgCliente.FecReg = TODAY.

    IF AgCliente.horAreg = ? THEN
        ASSIGN AgCliente.horAreg = INTEGER(TIME).

    IF AgCliente.id-user = ? THEN
        ASSIGN AgCliente.id-user = USERID('DICTDB').
END.
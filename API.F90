! ============================================
! Intergalactic Messaging Language API
! FORTRAN Module: IML_API
! ============================================

MODULE IML_API
  IMPLICIT NONE

  ! -- Message Type Constants
  INTEGER, PARAMETER :: SCIENTIFIC = 1
  INTEGER, PARAMETER :: PHILOSOPHICAL = 2
  INTEGER, PARAMETER :: EXISTENTIAL = 3
  INTEGER, PARAMETER :: ARTISTIC = 4
  INTEGER, PARAMETER :: EMERGENCY = 5

  ! -- IML Message Structure
  TYPE :: IML_Message
    CHARACTER(LEN=128) :: Origin_ID
    INTEGER            :: Message_Type
    CHARACTER(LEN=1024):: Payload
    CHARACTER(LEN=256) :: Signature
    REAL(8)            :: Timestamp
  END TYPE IML_Message

CONTAINS

  ! --- Create a new IML message ---
  FUNCTION Create_IML_Message(origin_id, msg_type, payload_text, signature_pattern, timestamp_value) RESULT(msg)
    CHARACTER(LEN=*), INTENT(IN) :: origin_id
    INTEGER, INTENT(IN)           :: msg_type
    CHARACTER(LEN=*), INTENT(IN)  :: payload_text
    CHARACTER(LEN=*), INTENT(IN)  :: signature_pattern
    REAL(8), INTENT(IN)           :: timestamp_value
    TYPE(IML_Message)             :: msg

    msg%Origin_ID = origin_id
    msg%Message_Type = msg_type
    msg%Payload = payload_text
    msg%Signature = signature_pattern
    msg%Timestamp = timestamp_value
  END FUNCTION Create_IML_Message

  ! --- Serialize IML message to string (for transmission) ---
  FUNCTION Serialize_IML_Message(msg) RESULT(serialized)
    TYPE(IML_Message), INTENT(IN) :: msg
    CHARACTER(LEN=1600) :: serialized

    WRITE(serialized, '(A,"|",I1,"|",A,"|",A,"|",F15.3)') TRIM(msg%Origin_ID), msg%Message_Type, TRIM(msg%Payload), TRIM(msg%Signature), msg%Timestamp
  END FUNCTION Serialize_IML_Message

  ! --- Deserialize string back to IML message (on reception) ---
  FUNCTION Deserialize_IML_Message(serialized) RESULT(msg)
    CHARACTER(LEN=*), INTENT(IN) :: serialized
    TYPE(IML_Message)            :: msg
    CHARACTER(LEN=128) :: origin
    CHARACTER(LEN=1024):: payload
    CHARACTER(LEN=256) :: signature
    INTEGER :: mtype
    REAL(8) :: tstamp

    READ(serialized, '(A,"|",I1,"|",A,"|",A,"|",F15.3)') origin, mtype, payload, signature, tstamp
    msg%Origin_ID = TRIM(origin)
    msg%Message_Type = mtype
    msg%Payload = TRIM(payload)
    msg%Signature = TRIM(signature)
    msg%Timestamp = tstamp
  END FUNCTION Deserialize_IML_Message

  ! --- Display message nicely ---
  SUBROUTINE Display_IML_Message(msg)
    TYPE(IML_Message), INTENT(IN) :: msg

    PRINT *, "--------------------------------------"
    PRINT *, "IML MESSAGE"
    PRINT *, "Origin ID      :", TRIM(msg%Origin_ID)
    PRINT *, "Message Type   :", Message_Type_To_String(msg%Message_Type)
    PRINT *, "Payload        :", TRIM(msg%Payload)
    PRINT *, "Signature      :", TRIM(msg%Signature)
    PRINT *, "Timestamp (s)  :", msg%Timestamp
    PRINT *, "--------------------------------------"
  END SUBROUTINE Display_IML_Message

  ! --- Helper: Message type to readable string ---
  FUNCTION Message_Type_To_String(type_id) RESULT(type_string)
    INTEGER, INTENT(IN) :: type_id
    CHARACTER(LEN=32)   :: type_string

    SELECT CASE (type_id)
    CASE (SCIENTIFIC)
      type_string = "Scientific"
    CASE (PHILOSOPHICAL)
      type_string = "Philosophical"
    CASE (EXISTENTIAL)
      type_string = "Existential"
    CASE (ARTISTIC)
      type_string = "Artistic"
    CASE (EMERGENCY)
      type_string = "Emergency"
    CASE DEFAULT
      type_string = "Unknown"
    END SELECT
  END FUNCTION Message_Type_To_String

END MODULE IML_API

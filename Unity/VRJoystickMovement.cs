using UnityEngine;
using UnityEngine.InputSystem;

public class VRJoystickMovement : MonoBehaviour
{
    public InputActionReference moveAction; // from Input System
    public Transform headset;               // reference to the VR camera (head)
    public float moveSpeed = 1.5f;

    private CharacterController characterController;

    void Start()
    {
        characterController = GetComponent<CharacterController>();
    }

    void Update()
    {
        Vector2 input = moveAction.action.ReadValue<Vector2>();

        // Project headset forward to the horizontal plane
        Vector3 forward = headset.forward;
        forward.y = 0;
        forward.Normalize();

        Vector3 right = headset.right;
        right.y = 0;
        right.Normalize();

        Vector3 move = forward * input.y + right * input.x;

        characterController.Move(move * moveSpeed * Time.deltaTime);
    }
}

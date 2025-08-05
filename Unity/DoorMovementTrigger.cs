using UnityEngine;

public class DoorMover : MonoBehaviour
{
    public GameObject player; // Reference to the player object
    public string moveAxis = "z"; // Choose "x", "y", or "z" to determine movement axis
    public float moveDistance = 1.5f; // Distance to move the player
    public bool lastDoor = false;

    private void OnTriggerStay(Collider other)
    {
        Debug.Log("Collision detected with: " + other.gameObject.name);
        Debug.Log(other);
        
        if (lastDoor == true)
        {
            Debug.Log("Ending game...");
            Application.Quit(); // Quits the game (only works in a built application)

            #if UNITY_EDITOR
            UnityEditor.EditorApplication.isPlaying = false; // Stops play mode in the editor
            #endif
        }

        if (other.CompareTag("Player") && player != null)
        {
            Debug.Log("Test");
            Vector3 newPosition = player.transform.position;
            
            switch (moveAxis.ToLower())
            {
                case "x":
                    newPosition.x += moveDistance;
                    break;
                case "y":
                    newPosition.y += moveDistance;
                    break;
                case "z":
                    newPosition.z += moveDistance;
                    break;
                default:
                    Debug.LogWarning("Invalid moveAxis value. Use 'x', 'y', or 'z'.");
                    return;
            }
            
            Debug.Log("Moving player to: " + newPosition);
            player.transform.position = newPosition;
        }
    }
}

using UnityEngine;
using UnityEngine.SceneManagement;

public class RoomLoader : MonoBehaviour
{
    public GameObject player; // Reference to the player object
    public string nextSzeneName; // Name der zu ladenden Szene
    public bool lastDoor = false;

    private void OnTriggerEnter(Collider other)
    {
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
            SceneManager.LoadSceneAsync(nextSzeneName, LoadSceneMode.Single);
        }
    }
}


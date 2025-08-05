using UnityEngine;
using System.IO;

public class PlayerMovementLoggerToCSV : MonoBehaviour
{
    public string fileSuffix = "Session1"; // Im Inspector einstellbar
    public GameObject target;

    private Vector3 lastPosition;
    private string filePath;

    void Start()
    {
        lastPosition = transform.position;

        // Dateiname mit Suffix
        string fileName = $"PlayerMovementLog_{fileSuffix}.csv";
        filePath = Path.Combine(Application.persistentDataPath, fileName);

        File.WriteAllText(filePath, "Zeit (s);Pos X;Pos Y;Pos Z;Bewegung X;Bewegung Y;Bewegung Z; Looking at Artwork;\n");

        Debug.Log("Logging to: " + filePath);
    }

    void Update()
    {
        Vector3 currentPosition = transform.position;
        Vector3 movement = currentPosition - lastPosition;

        // Blickrichtung des Spielers (forward vector)
        // Vector3 lookDirection = transform.forward;

        Vector3 origin = Camera.main.transform.position;
        Vector3 direction = Camera.main.transform.forward;

        RaycastHit hit;
        float maxDistance = 20f;
        string target = "none";

        if (Physics.Raycast(origin, direction, out hit, maxDistance))
        {
            if (hit.collider.tag.StartsWith("Artwork"))
            {
                Debug.Log("Looking at gaze target: " + hit.collider.tag);
                target = hit.collider.tag;
            }
        }

        string csvLine = string.Format(
            "{0:F2};{1:F2};{2:F2};{3:F2};{4:F2};{5:F2};{6:F2};{7:F2}",
            Time.time,
            currentPosition.x, currentPosition.y, currentPosition.z,
            movement.x, movement.y, movement.z,
            target
        );

        File.AppendAllText(filePath, csvLine + "\n");

        lastPosition = currentPosition;
    }
}

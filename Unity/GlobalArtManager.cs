using UnityEngine;
using UnityEngine.SceneManagement;
using System.Collections.Generic;
using System.IO;
using System.Text;

public class GlobalArtManager : MonoBehaviour
{
    public Texture[] artworks;
    private int currentArtIndex = 0;

    void Awake()
    {
        DontDestroyOnLoad(gameObject);
        SceneManager.sceneLoaded += OnSceneLoaded;

        // Optional: Shuffle artworks once at the start
        ShuffleArtworks();
    }

    void OnSceneLoaded(Scene scene, LoadSceneMode mode)
    {
        var allMeshes = FindObjectsOfType<MeshRenderer>();
        var frameList = new List<MeshRenderer>();
        foreach (var frame in allMeshes)
        {
            if (frame.name.ToLower().Contains("plane") &&
                frame.gameObject.tag.StartsWith("Artwork"))
            {
                frameList.Add(frame);
            }
        }

        Debug.Log("Assigning " + frameList.Count + " artworks in scene: " + scene.name);

        StringBuilder csvContent = new StringBuilder(); // To store CSV data
        csvContent.AppendLine("FrameName;ArtworkName"); // Add CSV header

        foreach (var frame in frameList)
        {
            if (currentArtIndex < artworks.Length)
            {
                Material matInstance = new Material(frame.material);
                matInstance.mainTexture = artworks[currentArtIndex];
                frame.material = matInstance;

                // Add frame and artwork info to CSV
                csvContent.AppendLine($"{frame.gameObject.tag};{artworks[currentArtIndex].name}");

                currentArtIndex++;
            }
            else
            {
                Debug.LogWarning("Not enough unique artworks for all frames!");
                break;
            }
        }

        // Write CSV to file
        string filePath = Path.Combine(Application.persistentDataPath, "AssignedArtworks.csv");
        File.WriteAllText(filePath, csvContent.ToString());
        Debug.Log("CSV file written to: " + filePath);
    }

    void ShuffleArtworks()
    {
        for (int i = 0; i < artworks.Length; i++)
        {
            Texture temp = artworks[i];
            int rand = Random.Range(i, artworks.Length);
            artworks[i] = artworks[rand];
            artworks[rand] = temp;
        }
    }
}
